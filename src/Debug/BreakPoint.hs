{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Debug.BreakPoint
  ( plugin
  , captureVars
  ) where

import           Control.Applicative ((<|>), empty)
import           Control.Arrow ((&&&))
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Writer.CPS
import           Data.Coerce
import           Data.Data
import qualified Data.Graph as Graph
import qualified Data.Map.Lazy as M
import           Data.Maybe
import           Data.Monoid (Any(..))
import           Data.Traversable (for)
import           GHC.Exts (TYPE, RuntimeRep(..), LiftedRep)
import qualified GHC.Tc.Plugin as Plugin

import qualified Debug.BreakPoint.GhcFacade as Ghc

import           Debug.Trace

captureVars :: M.Map String String
captureVars = mempty

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.renamedResultAction = const renameAction
  , Ghc.tcPlugin = const $ Just tcPlugin
  }

renameAction
  :: Ghc.TcGblEnv
  -> Ghc.HsGroup Ghc.GhcRn
  -> Ghc.TcM (Ghc.TcGblEnv, Ghc.HsGroup Ghc.GhcRn)
renameAction gblEnv group = do
  hscEnv <- Ghc.getTopEnv
  Ghc.Found _ breakPointMod <- liftIO $
    Ghc.findPluginModule hscEnv (Ghc.mkModuleName "Debug.BreakPoint")
  -- TODO this could fail if base if not a dependency?
  Ghc.Found _ preludeMod <- liftIO $
    Ghc.findImportedModule hscEnv (Ghc.mkModuleName "GHC.Show") (Just $ Ghc.fsLit "base")
  -- TODO This fails if 'containers' is not a dep
  Ghc.Found _ mapMod <- liftIO $
    Ghc.findImportedModule hscEnv (Ghc.mkModuleName "Data.Map.Internal") (Just $ Ghc.fsLit "containers")

  -- TODO cache these lookups somehow?
  captureVarsName <- Ghc.lookupOrig breakPointMod (Ghc.mkVarOcc "captureVars")
  showName <- Ghc.lookupOrig preludeMod (Ghc.mkVarOcc "show")
  fromListName <- Ghc.lookupOrig mapMod (Ghc.mkVarOcc "fromAscList")
  mkShowWrapperName <- Ghc.lookupOrig breakPointMod (Ghc.mkVarOcc "MkShowWrapper")

  let (group', _) =
        runReader (runWriterT $ recurse group)
          MkEnv { varSet = mempty, .. }

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
    <|> wrap hsDoCase
    <|> wrap hsProcCase
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
hsVarCase (Ghc.HsVar _ (Ghc.L _ name)) = do
  matchesTarget <- lift . asks $ (== name) . captureVarsName
  if matchesTarget
     then do
       nameShow <- lift $ asks showName
       fromListName <- lift $ asks fromListName
       names <- lift $ asks varSet
       showWrapperName <- lift $ asks mkShowWrapperName

       let mkTuple (Ghc.LexicalFastString varStr, n) =
             Ghc.mkLHsTupleExpr
               [ Ghc.nlHsLit . Ghc.mkHsString $ Ghc.unpackFS varStr
               , Ghc.nlHsApp (Ghc.nlHsVar nameShow) $
                   Ghc.nlHsApp (Ghc.nlHsVar showWrapperName) (Ghc.nlHsVar n)
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
matchCase Ghc.Match {..} = do
  let names = foldMap extractVarPats m_pats
  grhRes <- addScopedVars names $ recurse m_grhss
  pure $ Just
    Ghc.Match { Ghc.m_grhss = grhRes, .. }

extractVarPats :: Ghc.LPat Ghc.GhcRn -> VarSet
extractVarPats = mkVarSet . Ghc.collectPatBinders Ghc.CollNoDictBinders

--------------------------------------------------------------------------------
-- Guarded Right-hand Sides
--------------------------------------------------------------------------------

grhssCase :: Ghc.GRHSs Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
         -> EnvReader (Maybe (Ghc.GRHSs Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)))
grhssCase Ghc.GRHSs {..} = do
  (localBindsRes, names)
    <- dealWithLocalBinds grhssLocalBinds

  grhsRes <- addScopedVars names $ recurse grhssGRHSs
  pure $ Just
    Ghc.GRHSs { Ghc.grhssGRHSs = grhsRes, grhssLocalBinds = localBindsRes, .. }

dealWithBind :: VarSet
             -> (Ghc.LHsBind Ghc.GhcRn, [Ghc.Name])
             -> EnvReader (Ghc.LHsBind Ghc.GhcRn)
dealWithBind resultNames (lbind, names) = for lbind $ \bind -> do
  let resNameExcl = resultNames M.\\ mkVarSet names
  case bind of
    Ghc.FunBind {..} -> do
      (matchesRes, Any containsTarget)
        <- listen
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
      (rhsRes, Any containsTarget)
        <- listen
         . addScopedVars resNameExcl
         $ recurse pat_rhs
      let rhsVars
            | containsTarget
            = Ghc.mkUniqSet . M.elems
              . (<> resNameExcl) . mkVarSet
              $ Ghc.nonDetEltsUniqSet pat_ext
            | otherwise = pat_ext
      pure Ghc.PatBind { Ghc.pat_rhs = rhsRes, pat_ext = rhsVars, .. }

    -- Does this not occur in the renamer?
    Ghc.VarBind {..} -> do
      rhsRes
        <- addScopedVars resNameExcl
         $ recurse var_rhs
      pure Ghc.VarBind { Ghc.var_rhs = rhsRes, .. }

    Ghc.PatSynBind x Ghc.PSB {..} -> do
      (defRes, Any containsTarget)
        <- listen
         . addScopedVars resNameExcl
         $ recurse psb_def
      let rhsVars
            | containsTarget
            = Ghc.mkUniqSet . M.elems
              . (<> resNameExcl) . mkVarSet
              $ Ghc.nonDetEltsUniqSet psb_ext
            | otherwise = psb_ext
      pure $ Ghc.PatSynBind x Ghc.PSB { psb_def = defRes, psb_ext = rhsVars, .. }

    other -> pure other

grhsCase :: Ghc.GRHS Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
         -> EnvReader (Maybe (Ghc.GRHS Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)))
grhsCase (Ghc.GRHS x guards body) = do
  (guardsRes, names) <- runWriterT $ dealWithStatements guards
  bodyRes <- addScopedVars names $ recurse body
  pure . Just $ Ghc.GRHS x guardsRes bodyRes

--------------------------------------------------------------------------------
-- Let Binds (Non-do)
--------------------------------------------------------------------------------

-- TODO could combine with hsVar case to allow for "quick failure"
hsLetCase :: Ghc.HsExpr Ghc.GhcRn
          -> EnvReader (Maybe (Ghc.HsExpr Ghc.GhcRn))
hsLetCase (Ghc.HsLet x localBinds inExpr) = do
  (bindsRes, names) <- dealWithLocalBinds localBinds

  inExprRes <- addScopedVars names $ recurse inExpr
  pure . Just $
    Ghc.HsLet x bindsRes inExprRes
hsLetCase _ = pure Nothing

dealWithLocalBinds
  :: Ghc.HsLocalBinds Ghc.GhcRn
  -> EnvReader (Ghc.HsLocalBinds Ghc.GhcRn, VarSet)
dealWithLocalBinds = \case
  hlb@(Ghc.HsValBinds x valBinds) -> case valBinds of
    Ghc.ValBinds{} -> pure (hlb, mempty)
    Ghc.XValBindsLR (Ghc.NValBinds bindPairs sigs) -> do
      let binds = Ghc.bagToList
                . Ghc.unionManyBags
                $ map snd bindPairs :: [Ghc.LHsBind Ghc.GhcRn]
          names = map (foldMap $ Ghc.collectHsBindBinders Ghc.CollNoDictBinders)
                      binds
          resultNames = mkVarSet $ concat names

      let bindsWithNames = zip binds names

      (resBindsWithNames, Any containsTarget)
        <- listen
         . fmap (`zip` names)
         $ traverse (dealWithBind resultNames) bindsWithNames

      if not containsTarget
         then pure (hlb, resultNames) -- if no bind contained the target then we're done
         else do
           -- Need to reorder the binds because the variables references on the
           -- RHS of some binds have changed
           let mkTuple (bind, ns)
                 = (bind, ns, foldMap getRhsFreeVars bind)

               finalResult = depAnalBinds $ mkTuple <$> resBindsWithNames

           pure ( Ghc.HsValBinds x
                    $ Ghc.XValBindsLR
                        $ Ghc.NValBinds finalResult sigs
                , resultNames
                )

  x@(Ghc.HsIPBinds _ _) -> pure (x, mempty) -- TODO ImplicitParams

  other -> pure (other, mempty)

getRhsFreeVars :: Ghc.HsBind Ghc.GhcRn -> Ghc.UniqSet Ghc.Name
getRhsFreeVars = \case
  Ghc.FunBind {..} -> fun_ext
  Ghc.PatBind {..} -> pat_ext
  Ghc.PatSynBind _ Ghc.PSB {..} -> psb_ext
  _ -> mempty

--------------------------------------------------------------------------------
-- Do Block
--------------------------------------------------------------------------------

hsDoCase :: Ghc.HsExpr Ghc.GhcRn
         -> EnvReader (Maybe (Ghc.HsExpr Ghc.GhcRn))
-- TODO look at the context to determine if it's a recursive do
hsDoCase (Ghc.HsDo x ctx lStmts) = do
  (stmtsRes, _) <- runWriterT $ for lStmts dealWithStatements
  pure . Just $ Ghc.HsDo x ctx stmtsRes
hsDoCase _ = pure Nothing

dealWithStatements
  :: (Data body, Data (Ghc.Stmt Ghc.GhcRn body))
  => [Ghc.LStmt Ghc.GhcRn body]
  -> WriterT VarSet EnvReader [Ghc.LStmt Ghc.GhcRn body]
dealWithStatements [] = pure []
dealWithStatements (lstmt : xs) = do
  (stmtRes, names) <- listen $ traverse dealWithStmt lstmt
  (stmtRes :) <$> mapWriterT (addScopedVars names) (dealWithStatements xs)

dealWithStmt :: (Data (Ghc.Stmt Ghc.GhcRn body), Data body)
             => Ghc.Stmt Ghc.GhcRn body
             -> WriterT VarSet EnvReader (Ghc.Stmt Ghc.GhcRn body)
dealWithStmt = \case
  Ghc.BindStmt x lpat body -> do
    let names = extractVarPats lpat
    tell names
    bodyRes <- lift . addScopedVars names $ recurse body
    pure $ Ghc.BindStmt x lpat bodyRes

  Ghc.LetStmt x localBinds -> do
    (bindsRes, names) <- lift $ dealWithLocalBinds localBinds
    tell names
    pure $ Ghc.LetStmt x bindsRes

  Ghc.ApplicativeStmt x pairs mbJoin -> do
    let dealWithAppArg = \case
          a@Ghc.ApplicativeArgOne{..} -> do
            tell $ extractVarPats app_arg_pattern
            pure a
          a@Ghc.ApplicativeArgMany{..} -> do
            tell $ extractVarPats bv_pattern
            (stmtsRes, _) <- lift . runWriterT $ dealWithStatements app_stmts
            pure a {Ghc.app_stmts = stmtsRes}
    pairsRes <- (traverse . traverse) dealWithAppArg pairs
    pure $ Ghc.ApplicativeStmt x pairsRes mbJoin

  other -> lift $ gmapM recurse other

--------------------------------------------------------------------------------
-- Arrow Notation
--------------------------------------------------------------------------------

hsProcCase :: Ghc.HsExpr Ghc.GhcRn
           -> EnvReader (Maybe (Ghc.HsExpr Ghc.GhcRn))
hsProcCase (Ghc.HsProc x1 lpat cmdTop) = do
  let inputNames = extractVarPats lpat
  runMaybeT $ do
    cmdTopRes <- for cmdTop $ \case
      Ghc.HsCmdTop x2 lcmd -> do
        cmdRes <- for lcmd $ \case
          Ghc.HsCmdDo x3 lstmts -> do
            (stmtsRes, _) <- lift . runWriterT . for lstmts $ \stmts -> do
              tell inputNames
              mapWriterT (addScopedVars inputNames) $ dealWithStatements stmts
            pure $ Ghc.HsCmdDo x3 stmtsRes

          _other -> empty -- TODO what other cases should be handled?

        pure $ Ghc.HsCmdTop x2 cmdRes
    pure $ Ghc.HsProc x1 lpat cmdTopRes
hsProcCase _ = pure Nothing

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
  , captureVarsName :: !Ghc.Name
  , showName :: !Ghc.Name
  , fromListName :: !Ghc.Name
  , mkShowWrapperName :: !Ghc.Name
  }

overVarSet :: (VarSet -> VarSet) -> Env -> Env
overVarSet f env = env { varSet = f $ varSet env }

getOccNameFS :: Ghc.Name -> Ghc.LexicalFastString
getOccNameFS = Ghc.LexicalFastString . Ghc.occNameFS . Ghc.getOccName

mkVarSet :: [Ghc.Name] -> VarSet
mkVarSet names = M.fromList $ (getOccNameFS &&& id) <$> names

addScopedVars :: VarSet -> EnvReader a -> EnvReader a
addScopedVars names = mapWriterT $ local (overVarSet (names <>))

--------------------------------------------------------------------------------
-- Vendored from GHC
--------------------------------------------------------------------------------

depAnalBinds :: [(Ghc.LHsBind Ghc.GhcRn, [Ghc.Name], Ghc.UniqSet Ghc.Name)]
             -> [(Ghc.RecFlag, Ghc.LHsBinds Ghc.GhcRn)]
depAnalBinds binds_w_dus
  = map get_binds sccs
  where
    sccs = Ghc.depAnal
             (\(_, defs, _) -> defs)
             (\(_, _, uses) -> Ghc.nonDetEltsUniqSet uses)
             binds_w_dus

    get_binds (Graph.AcyclicSCC (bind, _, _)) = (Ghc.NonRecursive, Ghc.unitBag bind)
    get_binds (Graph.CyclicSCC  binds_w_dus')  = (Ghc.Recursive, Ghc.listToBag [b | (b,_,_) <- binds_w_dus'])

--------------------------------------------------------------------------------
-- Type Checker Plugin
--------------------------------------------------------------------------------

data TcPluginNames =
  MkTcPluginNames
    { showLevClassName :: !Ghc.Name
    , showClass :: !Ghc.Class
    -- , mkShowWrapperName :: !Ghc.Name
    }

tcPlugin :: Ghc.TcPlugin
tcPlugin = Ghc.TcPlugin
  { Ghc.tcPluginInit  = initTcPlugin
  , Ghc.tcPluginSolve = solver
  , Ghc.tcPluginStop = const $ pure ()
  }

initTcPlugin :: Ghc.TcPluginM TcPluginNames
initTcPlugin = do
  Ghc.Found _ breakPointMod <-
    Plugin.findImportedModule (Ghc.mkModuleName "Debug.BreakPoint") Nothing
  Ghc.Found _ showMod <-
    Plugin.findImportedModule (Ghc.mkModuleName "GHC.Show") (Just $ Ghc.fsLit "base")

  showLevClassName <- Plugin.lookupOrig breakPointMod (Ghc.mkClsOcc "ShowLev")
  -- mkShowWrapperName <- Plugin.lookupOrig breakPointMod (Ghc.mkVarOcc "MkShowWrapper")
  showClass <- Plugin.tcLookupClass =<< Plugin.lookupOrig showMod (Ghc.mkClsOcc "Show")

  pure MkTcPluginNames{..}

findShowLevWanted :: TcPluginNames -> Ghc.Ct -> Maybe (Either (Ghc.Type, Ghc.Ct) (Ghc.Type, Ghc.Ct))
findShowLevWanted names ct
  | Ghc.CDictCan{..} <- ct
  , showLevClassName names == Ghc.getName cc_class
  , [Ghc.TyConApp tyCon [], arg2] <- cc_tyargs
  = Just $ if Ghc.getName tyCon == Ghc.getName Ghc.liftedRepTyCon
       then Right (arg2, ct)
       else Left (arg2, ct)
  | otherwise = Nothing

solver :: TcPluginNames -> Ghc.TcPluginSolver
solver names given derived wanted = do
  solved <- for (findShowLevWanted names `mapMaybe` wanted) $ \case
    Left (ty, ct) -> do -- unlifted type
      unshowableDict <- Ghc.unsafeTcPluginTcM $ buildUnshowableDict ty
      pure (unshowableDict, ct)
    Right (ty, ct) -> do
      instEnvs <- Plugin.getInstEnvs
      case Ghc.lookupUniqueInstEnv instEnvs (showClass names) [ty] of
        Right (clsInst, tys) -> do
          let dfun = Ghc.is_dfun clsInst
              (vars, subclasses, insts) = Ghc.tcSplitSigmaTy $ Ghc.idType dfun
          if null subclasses
             then pure (Ghc.evDFunApp dfun [] [], ct)
             else do
               unshowableDict <- Ghc.unsafeTcPluginTcM $ buildUnshowableDict ty
               pure (unshowableDict, ct)
        Left _ -> do
          unshowableDict <- Ghc.unsafeTcPluginTcM $ buildUnshowableDict ty
          pure (unshowableDict, ct)
  traceM $ Ghc.showSDocUnsafe $ Ghc.ppr wanted
  pure $ Ghc.TcPluginOk solved []
-- If the rep is LiftedRep then call 'show' on it
-- otherwise make some string dependent on the runtime rep.
-- If searching for a Show instance for ShowWrapper, find some other means of
-- showing the value or make a message about the lack of Show instance.

buildUnshowableDict :: Ghc.Type -> Ghc.TcM Ghc.EvTerm
buildUnshowableDict ty = do
  str <- Ghc.mkStringExpr . Ghc.showSDocUnsafe $ Ghc.ppr ty
  pure . Ghc.EvExpr $ Ghc.mkCoreLams [Ghc.mkWildValBinder Ghc.oneDataConTy ty] str

--------------------------------------------------------------------------------
-- Showing
--------------------------------------------------------------------------------

-- | Levity polymorphic 'Show'
class ShowLev (rep :: RuntimeRep) (a :: TYPE rep) where
  showLev :: a -> String

newtype ShowWrapper a = MkShowWrapper a

instance ShowLev LiftedRep a => Show (ShowWrapper a)

-- Could be less work to have a newtype wrapper that derives Show and then only
-- need to handle the cases where a show instance is not found for something.
-- In this case would you only get wanted for things that truly don't have
-- Show instances or would you get them for things that have a super class
-- constraint as well?
