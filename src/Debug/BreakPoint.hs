{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
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
import           Control.Arrow ((&&&))
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Writer.CPS
import           Data.Coerce
import           Data.Data
import qualified Data.DList as DL
import           Data.Functor.Identity
import qualified Data.Generics as Syb
import qualified Data.Map.Lazy as M
import           Data.Traversable (for)

import qualified Debug.BreakPoint.GhcFacade as Ghc

import           Debug.Trace

traceVars :: M.Map String String
traceVars = mempty

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

  Ghc.Found _ mapMod <- liftIO $
    Ghc.findImportedModule hscEnv (Ghc.mkModuleName "Data.Map.Internal") (Just $ Ghc.fsLit "containers")

  -- TODO cache these lookups somehow?
  traceVarsName <- Ghc.lookupOrig breakPointMod (Ghc.mkVarOcc "traceVars")
  showName <- Ghc.lookupOrig preludeMod (Ghc.mkVarOcc "show")
  fromListName <- Ghc.lookupOrig mapMod (Ghc.mkVarOcc "fromAscList")

  let group' =
        runReader (recurse group)
          MkEnv { varSet = mempty
                , traceVarsName
                , showName
                , fromListName
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
    <|> wrap grhssCase
    <|> wrap hsLetCase
    <|> wrap grhsCase
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
  matchesTarget <- asks $ (== name) . traceVarsName
  if matchesTarget
     then do
       nameShow <- asks showName
       fromListName <- asks fromListName
       names <- asks varSet

       let mkTuple (Ghc.LexicalFastString varStr, name) =
             Ghc.mkLHsTupleExpr
               [ Ghc.nlHsLit . Ghc.mkHsString $ Ghc.unpackFS varStr
               , Ghc.nlHsApp (Ghc.nlHsVar nameShow) $ Ghc.nlHsVar name
               ]
               Ghc.NoExtField

           mkList exprs = Ghc.noLocA (Ghc.ExplicitList Ghc.NoExtField exprs)

           expr = Ghc.nlHsApp (Ghc.nlHsVar fromListName) . mkList
                $ mkTuple <$> M.toList names

       pure (Just $ Ghc.unLoc expr)
     else pure Nothing
hsVarCase _ = pure Nothing

--------------------------------------------------------------------------------
-- Match
--------------------------------------------------------------------------------

matchCase :: Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
          -> EnvReader (Maybe (Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)))
matchCase Ghc.Match {Ghc.m_ext, Ghc.m_ctxt, Ghc.m_pats, Ghc.m_grhss} = do
  let names = extractVarPats m_pats
  grhRes <- addScopedVars names $ recurse m_grhss
  pure $ Just
    Ghc.Match { Ghc.m_ext, Ghc.m_ctxt, Ghc.m_pats, Ghc.m_grhss = grhRes }

extractVarPats :: Data a => a -> VarSet
extractVarPats = Syb.everything (<>) (Syb.mkQ mempty findVarPats)
  where
    findVarPats :: Ghc.Pat Ghc.GhcRn -> VarSet
    findVarPats (Ghc.VarPat _ (Ghc.L _ name)) = mkVarSet [name]
    findVarPats _otherPat = mempty -- gmapQr (<>) mempty extractVarPats otherPat

--------------------------------------------------------------------------------
-- Guarded Right-hand Sides
--------------------------------------------------------------------------------

grhssCase :: Ghc.GRHSs Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
         -> EnvReader (Maybe (Ghc.GRHSs Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)))
grhssCase Ghc.GRHSs { Ghc.grhssExt, Ghc.grhssGRHSs, Ghc.grhssLocalBinds } = mdo
  (localBindsRes, names)
    <- runWriterT $ dealWithLocalBinds names grhssLocalBinds

  grhsRes <- addScopedVars names $ recurse grhssGRHSs
  pure $ Just
    Ghc.GRHSs { Ghc.grhssExt, Ghc.grhssGRHSs = grhsRes, Ghc.grhssLocalBinds }

dealWithBind :: VarSet
             -> Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn
             -> WriterT VarSet
                        EnvReader
                        (Ghc.HsBindLR Ghc.GhcRn Ghc.GhcRn)
dealWithBind resultNames = \case
  Ghc.FunBind {..} -> do
    let name = mkVarSet [Ghc.unLoc fun_id]
    tell name
    matchesRes <- lift . addScopedVars (resultNames M.\\ name)
                $ recurse fun_matches
    pure Ghc.FunBind { Ghc.fun_matches = matchesRes, .. }

  Ghc.PatBind {..} -> do
    let names = extractVarPats pat_lhs
    tell names
    rhsRes <- lift . addScopedVars (resultNames M.\\ names)
            $ recurse pat_rhs
    pure Ghc.PatBind { Ghc.pat_rhs = rhsRes, .. }

  Ghc.VarBind {..} -> do
    let name = mkVarSet [var_id]
    tell name
    rhsRes <- lift . addScopedVars (resultNames M.\\ name)
            $ recurse var_rhs
    pure Ghc.VarBind { Ghc.var_rhs = rhsRes, .. }

  Ghc.PatSynBind x Ghc.PSB {..} -> do
    let names = extractNames psb_args
    tell names
    defRes <- lift . addScopedVars (resultNames M.\\ names)
            $ recurse psb_def
    pure $ Ghc.PatSynBind x Ghc.PSB { psb_def = defRes, .. }

  other -> pure other

extractNames :: Data a => a -> VarSet
extractNames = mkVarSet . Syb.listify (const True)

grhsCase :: Ghc.GRHS Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
         -> EnvReader (Maybe (Ghc.GRHS Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)))
grhsCase (Ghc.GRHS x guards body) = do
  (guardsRes, names) <- runWriterT $ dealWithGuards guards
  bodyRes <- addScopedVars names $ recurse body
  pure . Just $ Ghc.GRHS x guardsRes bodyRes

dealWithGuards :: [Ghc.LStmt Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)]
               -> WriterT VarSet EnvReader [Ghc.LStmt Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)]
dealWithGuards [] = pure []
dealWithGuards (lstmt : rest) = mdo
  (lstmtRes, names) <- lift . runWriterT $
    for lstmt $ \case
      Ghc.LetStmt x localBinds -> do
        localBindsRes <- dealWithLocalBinds names localBinds
        pure $ Ghc.LetStmt x localBindsRes

      -- pattern guards
      Ghc.BindStmt x lpat body -> do
        let names = extractVarPats lpat
        tell names
        bodyRes <- lift $ recurse body
        pure $ Ghc.BindStmt x lpat bodyRes

      other -> pure other
  tell names
  (lstmtRes :) <$> mapWriterT (addScopedVars names) (dealWithGuards rest)

--------------------------------------------------------------------------------
-- Let Binds (Non-do)
--------------------------------------------------------------------------------

-- TODO combine with hsVar case to allow for "quick failure"
hsLetCase :: Ghc.HsExpr Ghc.GhcRn
          -> EnvReader (Maybe (Ghc.HsExpr Ghc.GhcRn))
hsLetCase (Ghc.HsLet x binds inExpr) = mdo
  (bindsRes, names) <- runWriterT $ dealWithLocalBinds names binds

  !_ <- traceM $ Ghc.showSDocUnsafe $ Ghc.ppr bindsRes
  -- TODO need to reorder the binds so that anything containing traceVars is
  -- at the end of the list. Will need to make the bindings recursive if there
  -- is more than one.
  -- This is because GHC orders the bindings according to their dependencies.
  -- Try finding the function that does this and using it here?
  -- depAnalBinds looks like it might be doing the sorting.
  inExprRes <- addScopedVars names $ recurse inExpr
  pure . Just $
    Ghc.HsLet x bindsRes inExprRes
hsLetCase _ = pure Nothing

-- TODO can use collectHsValBinders CollNoDictBinders to get the resultNames and ditch knot tying
-- and then have the name set to pass to rnLocalValBindsRHS
dealWithLocalBinds
  :: VarSet
  -> Ghc.HsLocalBinds Ghc.GhcRn
  -> WriterT VarSet EnvReader (Ghc.HsLocalBinds Ghc.GhcRn)
dealWithLocalBinds resultNames = \case
  hlb@(Ghc.HsValBinds x valBinds) -> case valBinds of
    Ghc.ValBinds{} -> pure hlb
    Ghc.XValBindsLR (Ghc.NValBinds bindPairs sigs) -> do
      bindPairsRes <-
        (traverse . traverse . traverse . traverse)
          (dealWithBind resultNames)
          bindPairs
      let input = Ghc.ValBinds _ (snd <$> bindPairsRes) sigs
      rnValBindsRHS
      binds_w_dus <-
        traverse
          (Ghc.rnLBind (Ghc.mkScopedTvFn sigs))
          (snd <$> bindsRes)
      let !(anal_binds, _) = Ghc.depAnalBinds binds_w_dus

      pure $ Ghc.HsValBinds x (Ghc.XValBindsLR (Ghc.NValBinds anal_binds sigs))
  x@(Ghc.HsIPBinds _ _) -> pure x -- TODO ImplicitParams
  other -> pure other

--------------------------------------------------------------------------------
-- Env
--------------------------------------------------------------------------------

type EnvReader = Reader Env

newtype OrderedSrcSpan = OrderedSrcSpan Ghc.SrcSpan
  deriving Eq

instance Ord OrderedSrcSpan where
  compare = coerce Ghc.leftmost_smallest

type VarSet = M.Map Ghc.LexicalFastString Ghc.Name

data Env = MkEnv
  { varSet :: !VarSet
  , traceVarsName :: !Ghc.Name
  , showName :: !Ghc.Name
  , fromListName :: !Ghc.Name
  }

overVarSet :: (VarSet -> VarSet) -> Env -> Env
overVarSet f env = env { varSet = f $ varSet env }

getOccNameFS :: Ghc.Name -> Ghc.LexicalFastString
getOccNameFS = Ghc.LexicalFastString . Ghc.occNameFS . Ghc.getOccName

mkVarSet :: [Ghc.Name] -> VarSet
mkVarSet names = M.fromList $ (getOccNameFS &&& id) <$> names

addScopedVars :: VarSet -> EnvReader a -> EnvReader a
addScopedVars names = local $ overVarSet (names <>)

-- Need to have access to Names
-- Need to build the var Map
-- Is it okay to use a state monad? Will it be a problem for knot tying?

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.renamedResultAction = const renameAction
  }
