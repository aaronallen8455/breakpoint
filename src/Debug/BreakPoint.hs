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
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Writer.CPS
import           Data.Coerce
import           Data.Data
import qualified Data.DList as DL
import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.Generics as Syb
import qualified Data.Graph as Graph
import qualified Data.Map.Lazy as M
import           Data.Monoid (Any(..))
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

  let (group', _) =
        runReader (runWriterT $ recurse group)
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
         -> MaybeT EnvReader a
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
  matchesTarget <- lift . asks $ (== name) . traceVarsName
  if matchesTarget
     then do
       nameShow <- lift $ asks showName
       fromListName <- lift $ asks fromListName
       names <- lift $ asks varSet

       let mkTuple (Ghc.LexicalFastString varStr, name) =
             Ghc.mkLHsTupleExpr
               [ Ghc.nlHsLit . Ghc.mkHsString $ Ghc.unpackFS varStr
               , Ghc.nlHsApp (Ghc.nlHsVar nameShow) $ Ghc.nlHsVar name
               ]
               Ghc.NoExtField

           mkList exprs = Ghc.noLocA (Ghc.ExplicitList Ghc.NoExtField exprs)

           expr = Ghc.nlHsApp (Ghc.nlHsVar fromListName) . mkList
                $ mkTuple <$> M.toList names

       tell $ Any True

       pure (Just $ Ghc.unLoc expr)
     else pure Nothing
hsVarCase _ = pure Nothing

--------------------------------------------------------------------------------
-- Match
--------------------------------------------------------------------------------

matchCase :: Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
          -> EnvReader (Maybe (Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)))
matchCase Ghc.Match {Ghc.m_ext, Ghc.m_ctxt, Ghc.m_pats, Ghc.m_grhss} = do
  let names = foldMap extractVarPats m_pats
  grhRes <- addScopedVars names $ recurse m_grhss
  pure $ Just
    Ghc.Match { Ghc.m_ext, Ghc.m_ctxt, Ghc.m_pats, Ghc.m_grhss = grhRes }

extractVarPats :: Ghc.LPat Ghc.GhcRn -> VarSet
extractVarPats = mkVarSet . Ghc.collectPatBinders Ghc.CollNoDictBinders

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
        resNameExcl = resultNames M.\\ name
    tell name
    (matchesRes, Any containsTarget)
      <- lift . listen
       . addScopedVars resNameExcl
       $ recurse fun_matches
    -- be sure to use the result names on the right so that they are overriden
    -- by any shadowing vars inside the expr.
    let rhsVars
          | containsTarget
          = Ghc.mkUniqSet . M.elems
            . (<> resNameExcl) . mkVarSet
            $ Ghc.nonDetEltsUniqSet fun_ext
          | otherwise = fun_ext
    pure Ghc.FunBind { Ghc.fun_matches = matchesRes, Ghc.fun_ext = rhsVars, .. }

  Ghc.PatBind {..} -> do
    let names = extractVarPats pat_lhs
        resNameExcl = resultNames M.\\ names
    tell names
    (rhsRes, Any containsTarget)
      <- lift . listen
       . addScopedVars resNameExcl
       $ recurse pat_rhs
    let rhsVars
          | containsTarget
          = Ghc.mkUniqSet . M.elems
            . (<> resNameExcl) . mkVarSet
            $ Ghc.nonDetEltsUniqSet pat_ext
    pure Ghc.PatBind { Ghc.pat_rhs = rhsRes, pat_ext = rhsVars, .. }

  -- Does this not occur in the renamer?
  Ghc.VarBind {..} -> do
    let name = mkVarSet [var_id]
    tell name
    rhsRes
      <- lift
       . addScopedVars (resultNames M.\\ name)
       $ recurse var_rhs
    pure Ghc.VarBind { Ghc.var_rhs = rhsRes, .. }

  Ghc.PatSynBind x Ghc.PSB {..} -> do
    let names = extractNames psb_args
        resNameExcl = resultNames M.\\ names
    tell names
    (defRes, Any containsTarget)
      <- lift . listen
       . addScopedVars resNameExcl
       $ recurse psb_def
    let rhsVars
          | containsTarget
          = Ghc.mkUniqSet . M.elems
            . (<> resNameExcl) . mkVarSet
            $ Ghc.nonDetEltsUniqSet psb_ext
    pure $ Ghc.PatSynBind x Ghc.PSB { psb_def = defRes, psb_ext = rhsVars, .. }

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
  (lstmtRes, names) <- listen $
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
  (lstmtRes :) <$> mapWriterT (addScopedVars names) (dealWithGuards rest)

--------------------------------------------------------------------------------
-- Let Binds (Non-do)
--------------------------------------------------------------------------------

-- TODO combine with hsVar case to allow for "quick failure"
hsLetCase :: Ghc.HsExpr Ghc.GhcRn
          -> EnvReader (Maybe (Ghc.HsExpr Ghc.GhcRn))
hsLetCase (Ghc.HsLet x binds inExpr) = mdo
  (bindsRes, names) <- runWriterT $ dealWithLocalBinds names binds

  inExprRes <- addScopedVars names $ recurse inExpr
  pure . Just $
    Ghc.HsLet x bindsRes inExprRes
hsLetCase _ = pure Nothing

dealWithLocalBinds
  :: VarSet
  -> Ghc.HsLocalBinds Ghc.GhcRn
  -> WriterT VarSet EnvReader (Ghc.HsLocalBinds Ghc.GhcRn)
dealWithLocalBinds resultNames = \case
  hlb@(Ghc.HsValBinds x valBinds) -> case valBinds of
    Ghc.ValBinds{} -> pure hlb
    Ghc.XValBindsLR (Ghc.NValBinds bindPairs sigs) -> do
      let binds = Ghc.unionManyBags $ map snd bindPairs :: Ghc.Bag (Ghc.LHsBind Ghc.GhcRn)
          rearrange ((a, w1), w2) = ((a, w2), w1)

      (resBindsWithNames, Any containsTarget)
        <- mapWriterT (fmap rearrange . listen)
         $ traverse (listen . traverse (dealWithBind resultNames)) binds

      if not containsTarget
         then pure hlb -- if no bind contained the target then we're done
         else do
           -- Need to reorder the binds because the variables references on the
           -- RHS of some binds have changed
           let mkTuple (bind, names)
                 = (bind, M.elems names, foldMap getRhsFreeVars bind)
               finalResult = depAnalBinds $ mkTuple <$> resBindsWithNames
           pure $ Ghc.HsValBinds x
                    $ Ghc.XValBindsLR
                        $ Ghc.NValBinds finalResult sigs

  x@(Ghc.HsIPBinds _ _) -> pure x -- TODO ImplicitParams

  other -> pure other

getRhsFreeVars :: Ghc.HsBind Ghc.GhcRn -> Ghc.UniqSet Ghc.Name
getRhsFreeVars = \case
  Ghc.FunBind {..} -> fun_ext
  Ghc.PatBind {..} -> pat_ext
  Ghc.PatSynBind _ Ghc.PSB {..} -> psb_ext
  _ -> mempty

--------------------------------------------------------------------------------
-- Env
--------------------------------------------------------------------------------

-- The writer is for tracking if an inner expression contains the target name
type EnvReader = WriterT Any (Reader Env)

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
addScopedVars names = mapWriterT $ local (overVarSet (names <>))

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.renamedResultAction = const renameAction
  }

--------------------------------------------------------------------------------
-- Vendored from GHC
--------------------------------------------------------------------------------

depAnalBinds :: Ghc.Bag (Ghc.LHsBind Ghc.GhcRn, [Ghc.Name], Ghc.UniqSet Ghc.Name)
             -> [(Ghc.RecFlag, Ghc.LHsBinds Ghc.GhcRn)]
depAnalBinds binds_w_dus
  = map get_binds sccs
  where
    sccs = Ghc.depAnal
             (\(_, defs, _) -> defs)
             (\(_, _, uses) -> Ghc.nonDetEltsUniqSet uses)
             (Ghc.bagToList binds_w_dus)

    get_binds (Graph.AcyclicSCC (bind, _, _)) = (Ghc.NonRecursive, Ghc.unitBag bind)
    get_binds (Graph.CyclicSCC  binds_w_dus)  = (Ghc.Recursive, Ghc.listToBag [b | (b,_,_) <- binds_w_dus])
