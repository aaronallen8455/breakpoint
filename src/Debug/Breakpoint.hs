{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Debug.Breakpoint
  ( plugin
  , captureVars
  , showLev
  , fromAscList
  , printAndWait
  , printAndWaitM
  , printAndWaitIO
  , bp
  , bpM
  , bpIO
  , getSrcLoc
  ) where

import           Control.Applicative ((<|>), empty)
import           Control.Arrow ((&&&))
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Writer.CPS
import           Data.Coerce
import           Data.Data hiding (IntRep, FloatRep)
import           Data.Either
import           Data.Functor
import qualified Data.Graph as Graph
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid (Any(..))
import           Data.Traversable (for)
import           Debug.Trace (traceIO)
import           GHC.Exts (TYPE, RuntimeRep(..), LiftedRep, Int#, Float(..), Float#, Double#, Double(..), Int8#, Int16#, Int32#, Word#, Word8#, Word16#, Word32#)
import           GHC.Int (Int(..), Int8(..), Int16(..), Int32(..))
import qualified GHC.Tc.Plugin as Plugin
import           GHC.Word (Word(..), Word8(..), Word16(..), Word32(..))
import           System.IO.Unsafe (unsafePerformIO)

import qualified Debug.Breakpoint.GhcFacade as Ghc

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

-- | Constructs a lazy 'Map' from the names of all visible variables at the call
-- site to a string representation of their value.
captureVars :: M.Map String String
captureVars = mempty

-- re-exported to avoid requiring the client to depend on the containers package
fromAscList :: Ord k => [(k, v)] -> M.Map k v
fromAscList = M.fromAscList

printAndWait :: String -> M.Map String String -> a -> a
printAndWait srcLoc vars x =
  unsafePerformIO $ printAndWaitIO srcLoc vars >> pure x
{-# NOINLINE printAndWait #-}

printAndWaitM :: Applicative m => String -> M.Map String String -> m ()
printAndWaitM srcLoc vars = printAndWait srcLoc vars $ pure ()

printAndWaitIO :: MonadIO m => String -> M.Map String String -> m ()
printAndWaitIO srcLoc vars = liftIO $ do
  traceIO "### Breakpoint Hit ###"
  traceIO srcLoc
  printVars vars
  traceIO "Press enter to continue"
  _ <- getLine
  pure ()

-- | Sets a breakpoint in pure code
bp :: a -> a
bp = id

-- | Sets a breakpoint in an arbitrary 'Applicative'. Uses 'unsafePerformIO'
-- which means that laziness and common sub-expression elimination can result
-- in the breakpoint not being hit as expected. For this reason, you should
-- prefer 'bpIO' if a `MonadIO` instance is available.
bpM :: Applicative m => m ()
bpM = pure ()

-- | Sets a breakpoint in an 'IO' based 'Monad'. You should favor this over
-- 'bpM' if the monad can perform IO.
bpIO :: MonadIO m => m ()
bpIO = pure ()

getSrcLoc :: String
getSrcLoc = ""

printVars :: M.Map String String -> IO ()
printVars vars = do
  let mkLine (k, v) = k <> " = " <> v
  traceIO . unlines $ mkLine <$> M.toList vars

--------------------------------------------------------------------------------
-- Plugin
--------------------------------------------------------------------------------

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
  Ghc.Found _ breakpointMod <- liftIO $
    Ghc.findPluginModule hscEnv (Ghc.mkModuleName "Debug.Breakpoint")

  captureVarsName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "captureVars")
  showLevName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "showLev")
  fromListName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "fromAscList")
  bpName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "bp")
  bpMName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "bpM")
  bpIOName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "bpIO")
  printAndWaitName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "printAndWait")
  printAndWaitMName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "printAndWaitM")
  printAndWaitIOName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "printAndWaitIO")
  getSrcLocName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "getSrcLoc")

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
hsVarCase (Ghc.HsVar _ (Ghc.L loc name)) = do
  MkEnv{..} <- lift ask

  let srcLocStringExpr
        = Ghc.nlHsLit . Ghc.mkHsString
        . (\x -> "(" <> x <> ")")
        . Ghc.showSDocUnsafe
        . Ghc.pprUserSpan True
        $ Ghc.locA loc

      captureVarsExpr =
        let mkTuple (Ghc.LexicalFastString varStr, n) =
              Ghc.mkLHsTupleExpr
                [ Ghc.nlHsLit . Ghc.mkHsString $ Ghc.unpackFS varStr
                , Ghc.nlHsApp (Ghc.nlHsVar showLevName) (Ghc.nlHsVar n)
                ]
                Ghc.NoExtField

            mkList exprs = Ghc.noLocA (Ghc.ExplicitList Ghc.NoExtField exprs)

         in Ghc.nlHsApp (Ghc.nlHsVar fromListName) . mkList
              $ mkTuple <$> M.toList varSet

      bpExpr =
        Ghc.nlHsApp
          (Ghc.nlHsApp (Ghc.nlHsVar printAndWaitName) srcLocStringExpr)
          captureVarsExpr

      bpMExpr =
        Ghc.nlHsApp
          (Ghc.nlHsApp (Ghc.nlHsVar printAndWaitMName) srcLocStringExpr)
          captureVarsExpr

      bpIOExpr =
        Ghc.nlHsApp
          (Ghc.nlHsApp (Ghc.nlHsVar printAndWaitIOName) srcLocStringExpr)
          captureVarsExpr

  if | captureVarsName == name -> do
         tell $ Any True
         pure (Just $ Ghc.unLoc captureVarsExpr)

     | bpName == name -> do
         tell $ Any True
         pure (Just $ Ghc.unLoc bpExpr)

     | bpMName == name -> do
         tell $ Any True
         pure (Just $ Ghc.unLoc bpMExpr)

     | bpIOName == name -> do
         tell $ Any True
         pure (Just $ Ghc.unLoc bpIOExpr)

     | getSrcLocName == name ->
         pure (Just $ Ghc.unLoc srcLocStringExpr)

     | otherwise -> pure Nothing
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

          _ -> empty -- TODO what other cases should be handled?

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
  , showLevName :: !Ghc.Name
  , fromListName :: !Ghc.Name
  , bpName :: !Ghc.Name
  , bpMName :: !Ghc.Name
  , bpIOName :: !Ghc.Name
  , printAndWaitName :: !Ghc.Name
  , printAndWaitMName :: !Ghc.Name
  , printAndWaitIOName :: !Ghc.Name
  , getSrcLocName :: !Ghc.Name
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

    get_binds (Graph.AcyclicSCC (bind, _, _)) =
      (Ghc.NonRecursive, Ghc.unitBag bind)
    get_binds (Graph.CyclicSCC  binds_w_dus') =
      (Ghc.Recursive, Ghc.listToBag [b | (b,_,_) <- binds_w_dus'])

--------------------------------------------------------------------------------
-- Type Checker Plugin
--------------------------------------------------------------------------------

data TcPluginNames =
  MkTcPluginNames
    { showLevClassName :: !Ghc.Name
    , showClass :: !Ghc.Class
    , succeedClass :: !Ghc.Class
    , showWrapperTyCon :: !Ghc.TyCon
    }

tcPlugin :: Ghc.TcPlugin
tcPlugin = Ghc.TcPlugin
  { Ghc.tcPluginInit  = initTcPlugin
  , Ghc.tcPluginSolve = solver
  , Ghc.tcPluginStop = const $ pure ()
  }

initTcPlugin :: Ghc.TcPluginM TcPluginNames
initTcPlugin = do
  Ghc.Found _ breakpointMod <-
    Plugin.findImportedModule (Ghc.mkModuleName "Debug.Breakpoint") Nothing
  Ghc.Found _ showMod <-
    Plugin.findImportedModule (Ghc.mkModuleName "GHC.Show") (Just $ Ghc.fsLit "base")

  showLevClassName <- Plugin.lookupOrig breakpointMod (Ghc.mkClsOcc "ShowLev")
  showClass <- Plugin.tcLookupClass =<< Plugin.lookupOrig showMod (Ghc.mkClsOcc "Show")
  succeedClass <- Plugin.tcLookupClass =<< Plugin.lookupOrig breakpointMod (Ghc.mkClsOcc "Succeed")
  showWrapperTyCon <- Plugin.tcLookupTyCon =<< Plugin.lookupOrig breakpointMod (Ghc.mkClsOcc "ShowWrapper")

  pure MkTcPluginNames{..}

findShowLevWanted
  :: TcPluginNames
  -> Ghc.Ct
  -> Maybe (Either (Ghc.Type, Ghc.Ct) (Ghc.Type, Ghc.Ct))
findShowLevWanted names ct
  | Ghc.CDictCan{..} <- ct
  , showLevClassName names == Ghc.getName cc_class
  , [Ghc.TyConApp tyCon [], arg2] <- cc_tyargs
  = Just $ if Ghc.getName tyCon == Ghc.getName Ghc.liftedRepTyCon
       then Right (arg2, ct)
       else Left (arg2, ct)
  | otherwise = Nothing

solver :: TcPluginNames -> Ghc.TcPluginSolver
solver names _given _derived wanted = do
  instEnvs <- Plugin.getInstEnvs
  solved <- for (findShowLevWanted names `mapMaybe` wanted) $ \case
    Left (ty, ct) -> do -- unlifted type
      unshowableDict <- Ghc.unsafeTcPluginTcM $ buildUnshowableDict ty
      pure $ Just (unshowableDict, ct)
    Right (ty, ct) -> do
      mShowDict <- buildDict names (showClass names) [ty]
      pure $ mShowDict <&> \showDict ->
        let (succInst, _) = fromRight (error "impossible: no Succeed instance") $
              Ghc.lookupUniqueInstEnv instEnvs (succeedClass names) [ty]
         in (liftDict succInst ty (getEvExprFromDict showDict), ct)
  pure $ Ghc.TcPluginOk (catMaybes solved) []

buildDict
  :: TcPluginNames
  -> Ghc.Class
  -> [Ghc.Type]
  -> Ghc.TcPluginM (Maybe Ghc.EvTerm)
buildDict names cls tys = do
  instEnvs <- Plugin.getInstEnvs
  case Ghc.lookupUniqueInstEnv instEnvs (showClass names) tys of
    Right (clsInst, _) -> do
      let dfun = Ghc.is_dfun clsInst
          (vars, subclasses, inst) = Ghc.tcSplitSigmaTy $ Ghc.idType dfun
      if null subclasses
         then pure . Just $ Ghc.evDFunApp dfun [] [] -- why no use of vars here?
         else do
           let tyVarMap = mkTyVarMapping inst tys
           mSolvedSubClassDicts <- fmap sequence . for subclasses $ \subclass -> do
             let (subCls, subTys) = Ghc.tcSplitDFunHead subclass
                 subTys' = instantiateVars tyVarMap subTys
             buildDict names subCls subTys'
           pure $ do
             vars' <- traverse (tyVarMap M.!?) vars
             Ghc.evDFunApp dfun vars' . map getEvExprFromDict
               <$> mSolvedSubClassDicts
    Left _
      | cls == showClass names
      , [ty] <- tys -> do
          unshowableDict <- Ghc.unsafeTcPluginTcM $ buildUnshowableDict ty
          let (inst, _) = fromRight (error "impossible: no Show instance for ShowWrapper") $
                Ghc.lookupUniqueInstEnv
                  instEnvs
                  cls
                  [Ghc.mkTyConApp (showWrapperTyCon names) [ty]]
              liftedDict =
                liftDict inst ty (getEvExprFromDict unshowableDict)
          pure $ Just liftedDict
      | otherwise -> pure Nothing

getEvExprFromDict :: Ghc.EvTerm -> Ghc.EvExpr
getEvExprFromDict = \case
  Ghc.EvExpr expr -> expr
  _ -> error "invalid argument to getEvExprFromDict"

mkTyVarMapping
  :: Ghc.Type -- Wanted instance
  -> [Ghc.Type] -- Concrete types
  -> M.Map Ghc.TyVar Ghc.Type
mkTyVarMapping wanted tys =
  let wantedHead = snd $ Ghc.splitAppTys wanted
      wantedTyVars = concatMap (snd . Ghc.splitAppTys) wantedHead
      concreteTys = concatMap (snd . Ghc.splitAppTys) tys
   in M.fromList $ do
     (a, b) <- zip wantedTyVars concreteTys
     Just tyVar <- [Ghc.getTyVar_maybe a]
     pure (tyVar, b)

instantiateVars :: M.Map Ghc.TyVar Ghc.Type -> [Ghc.Type] -> [Ghc.Type]
instantiateVars tyVarMap tys = replace <$> tys
  where
    replace arg = fromMaybe arg $ do
      tyVar <- Ghc.getTyVar_maybe arg
      M.lookup tyVar tyVarMap -- this lookup shouldn't fail

buildUnshowableDict :: Ghc.Type -> Ghc.TcM Ghc.EvTerm
buildUnshowableDict ty = do
  let tyString = Ghc.showSDocUnsafe $ Ghc.pprTypeForUser ty
  str <- Ghc.mkStringExpr $ "<" <> tyString <> ">"
  pure . Ghc.EvExpr $
    Ghc.mkCoreLams [Ghc.mkWildValBinder Ghc.oneDataConTy ty] str

liftDict :: Ghc.ClsInst -> Ghc.Type -> Ghc.EvExpr -> Ghc.EvTerm
liftDict succ_inst ty dict = Ghc.evDFunApp (Ghc.is_dfun succ_inst) [ty] [dict]

--------------------------------------------------------------------------------
-- Showing
--------------------------------------------------------------------------------

-- | Levity polymorphic 'Show'
class ShowLev (rep :: RuntimeRep) (a :: TYPE rep) where
  showLev :: a -> String

instance ShowLev 'IntRep Int# where
  showLev i = show $ I# i

instance ShowLev 'Int8Rep Int8# where
  showLev i = show $ I8# i

instance ShowLev 'Int16Rep Int16# where
  showLev i = show $ I16# i

instance ShowLev 'Int32Rep Int32# where
  showLev i = show $ I32# i

instance ShowLev 'WordRep Word# where
  showLev w = show $ W# w

instance ShowLev 'Word8Rep Word8# where
  showLev w = show $ W8# w

instance ShowLev 'Word16Rep Word16# where
  showLev w = show $ W16# w

instance ShowLev 'Word32Rep Word32# where
  showLev w = show $ W32# w

instance ShowLev 'FloatRep Float# where
  showLev f = show $ F# f

instance ShowLev 'DoubleRep Double# where
  showLev d = show $ D# d

newtype ShowWrapper a = MkShowWrapper a

instance ShowLev LiftedRep a => Show (ShowWrapper a) where
  show (MkShowWrapper a) = showLev a

class Succeed a where
  _succeed :: a -> String

instance Show a => Succeed a where
  _succeed = show
