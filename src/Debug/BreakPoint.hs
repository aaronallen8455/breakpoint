{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
module Debug.BreakPoint
  ( plugin
  , traceVars
  ) where

import           Control.Applicative ((<|>), empty)
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.Coerce
import           Data.Data
import qualified Data.DList as DL
import           Data.Functor.Identity
import qualified Data.Generics as Syb
import qualified Data.Map as M

import qualified Debug.BreakPoint.GhcFacade as Ghc

import           Debug.Trace

traceVars :: [String]
traceVars = []

renameAction
  :: Ghc.TcGblEnv
  -> Ghc.HsGroup Ghc.GhcRn
  -> Ghc.TcM (Ghc.TcGblEnv, Ghc.HsGroup Ghc.GhcRn)
renameAction gblEnv group = do
  hscEnv <- Ghc.getTopEnv
  Ghc.Found _ breakPointMod <- liftIO $
    Ghc.findImportedModule hscEnv (Ghc.mkModuleName "Debug.BreakPoint") Nothing
  -- TODO this could fail if base if not a dependency?
  Ghc.Found _ preludeMod <- liftIO $
    Ghc.findImportedModule hscEnv (Ghc.mkModuleName "GHC.Show") (Just $ Ghc.fsLit "base")

  -- TODO cache these lookups somehow?
  traceVarsName <- Ghc.lookupOrig breakPointMod (Ghc.mkVarOcc "traceVars")
  showName <- Ghc.lookupOrig preludeMod (Ghc.mkVarOcc "show")

  let group' =
        runReader (recurse group)
          MkEnv { varSet = mempty
                , targetName = traceVarsName
                , showName = showName
                }
  pure (gblEnv, group')

recurse :: Data a => a -> EnvReader a
recurse a =
  maybe (gmapM recurse a) pure
    =<< transform a

newtype T a = T (a -> EnvReader (Maybe a))

transform :: forall a. Data a => a -> EnvReader (Maybe a)
transform a = runMaybeT
      $ wrap hsVarCase
    <|> wrap matchCase
    <|> wrap grhsCase
    <|> wrap hsLetCase
  where
    wrap :: forall b. Data b
         => (b -> EnvReader (Maybe b))
         -> MaybeT (Reader Env) a
    wrap f = do
      case gcast @b @a (T f) of
        Nothing -> empty
        Just (T f') -> MaybeT $ f' a

--------------------------------------------------------------------------------
-- Variable Expr
--------------------------------------------------------------------------------

hsVarCase :: Ghc.HsExpr Ghc.GhcRn
          -> EnvReader (Maybe (Ghc.HsExpr Ghc.GhcRn))
hsVarCase (Ghc.HsVar _ (Ghc.L srcSpanAnn name)) = do
  matchesTarget <- asks $ (== name) . targetName
  if matchesTarget
     then do
       nameShow <- asks showName
       names <- asks varSet

       let expr = Ghc.ExplicitList Ghc.noExtField
                $ Ghc.nlHsApp (Ghc.nlHsVar nameShow) . Ghc.nlHsVar
              <$> names

       pure (Just expr)
     else pure Nothing
hsVarCase _ = pure Nothing

--------------------------------------------------------------------------------
-- Match
--------------------------------------------------------------------------------

matchCase :: Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
          -> EnvReader (Maybe (Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)))
matchCase Ghc.Match {Ghc.m_ext, Ghc.m_ctxt, Ghc.m_pats, Ghc.m_grhss} = do
  let names = DL.toList $ extractVarPats m_pats
  grhRes <- addScopedVars names $ recurse m_grhss
  pure $ Just
    Ghc.Match { Ghc.m_ext, Ghc.m_ctxt, Ghc.m_pats, Ghc.m_grhss = grhRes }

extractVarPats :: Data a => a -> DL.DList Ghc.Name
extractVarPats = Syb.everything (<>) (Syb.mkQ mempty findVarPats)
  where
    findVarPats :: Ghc.Pat Ghc.GhcRn -> DL.DList Ghc.Name
    findVarPats (Ghc.VarPat _ (Ghc.L _ name)) = pure name
    findVarPats _otherPat = mempty -- gmapQr (<>) mempty extractVarPats otherPat

--------------------------------------------------------------------------------
-- Guarded Right-hand Sides
--------------------------------------------------------------------------------

grhsCase :: Ghc.GRHSs Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
         -> EnvReader (Maybe (Ghc.GRHSs Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)))
grhsCase Ghc.GRHSs { Ghc.grhssExt, Ghc.grhssGRHSs, Ghc.grhssLocalBinds } = do
  let names = DL.toList
            $ Syb.everything (<>) (Syb.mkQ mempty getLHSofBind) grhssLocalBinds
  grhsRes <- addScopedVars names $ recurse grhssGRHSs
  pure $ Just
    Ghc.GRHSs { Ghc.grhssExt, Ghc.grhssGRHSs = grhsRes, Ghc.grhssLocalBinds }

-- TODO also need to recurse over the RHS, excluding each LHS from its own scope.
-- Although perhaps it's safer to not let bindings see each other because of
-- possible mutual recursion, that's on the user though, so it should be justified.
--
-- Might be tricky to remove the self reference in the case of variable shadowing,
-- maybe that is ok though? Would having a Map keyed on strings solve the shadowing
-- problems all together?
getLHSofBind :: Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn -> DL.DList Ghc.Name
getLHSofBind = \case
  Ghc.FunBind { Ghc.fun_id } -> pure $ Ghc.unLoc fun_id
  Ghc.PatBind { Ghc.pat_lhs } -> extractVarPats pat_lhs
  Ghc.VarBind { Ghc.var_id } -> pure var_id
  Ghc.PatSynBind _ Ghc.PSB { Ghc.psb_args } -> extractNames psb_args

extractNames :: Data a => a -> DL.DList Ghc.Name
extractNames = DL.fromList . Syb.listify (const True)

--------------------------------------------------------------------------------
-- Let Binds (Non-do)
--------------------------------------------------------------------------------

hsLetCase :: Ghc.HsExpr Ghc.GhcRn
            -> EnvReader (Maybe (Ghc.HsExpr Ghc.GhcRn))
hsLetCase (Ghc.HsLet x binds lhs) = do
  let names = DL.toList
            $ Syb.everything (<>) (Syb.mkQ mempty getLHSofBind) binds
  lhsRes <- addScopedVars names $ recurse lhs
  pure . Just $
    Ghc.HsLet x binds lhsRes
hsLetCase _ = pure Nothing

--------------------------------------------------------------------------------
-- Env
--------------------------------------------------------------------------------

type EnvReader a = Reader Env a

newtype OrderedSrcSpan = OrderedSrcSpan Ghc.SrcSpan
  deriving Eq

instance Ord OrderedSrcSpan where
  compare = coerce Ghc.leftmost_smallest

type VarSet = [Ghc.Name]

data Env = MkEnv
  { varSet :: !VarSet
  , targetName :: !Ghc.Name
  , showName :: !Ghc.Name
  }

overVarSet :: (VarSet -> VarSet) -> Env -> Env
overVarSet f env = env { varSet = f $ varSet env }

addScopedVars :: [Ghc.Name] -> EnvReader a -> EnvReader a
addScopedVars names = local (overVarSet (names ++))

-- Need to have access to Names
-- Need to build the var Map
-- Is it okay to use a state monad? Will it be a problem for knot tying?

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.renamedResultAction = const renameAction
  }
