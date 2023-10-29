{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Debug.Breakpoint.TypeChecker
  ( tcPlugin
  ) where

import           Data.Either
import           Data.Maybe
import           Data.Traversable (for)
import qualified GHC.Tc.Plugin as Plugin

import qualified Debug.Breakpoint.GhcFacade as Ghc

--------------------------------------------------------------------------------
-- Type Checker Plugin
--------------------------------------------------------------------------------

data TcPluginNames =
  MkTcPluginNames
    { showLevClassName :: !Ghc.Name
    , showLevNameTc :: !Ghc.Name
    , showClass :: !Ghc.Class
    , succeedClass :: !Ghc.Class
    , showWrapperTyCon :: !Ghc.TyCon
    }

tcPlugin :: Ghc.TcPlugin
tcPlugin = Ghc.TcPlugin
  { Ghc.tcPluginInit  = initTcPlugin
  , Ghc.tcPluginSolve = solver
  , Ghc.tcPluginStop = const $ pure ()
#if MIN_VERSION_ghc(9,4,0)
  , Ghc.tcPluginRewrite = mempty
#endif
  }

initTcPlugin :: Ghc.TcPluginM TcPluginNames
initTcPlugin = do
  Ghc.Found _ breakpointMod <-
    Ghc.findImportedModule' (Ghc.mkModuleName "Debug.Breakpoint")
  Ghc.Found _ showMod <-
    Ghc.findImportedModule' (Ghc.mkModuleName "GHC.Show")

  showLevClassName <- Plugin.lookupOrig breakpointMod (Ghc.mkClsOcc "ShowLev")
  showLevNameTc <- Plugin.lookupOrig breakpointMod (Ghc.mkVarOcc "showLev")
  showClass <- Plugin.tcLookupClass =<< Plugin.lookupOrig showMod (Ghc.mkClsOcc "Show")
  succeedClass <- Plugin.tcLookupClass =<< Plugin.lookupOrig breakpointMod (Ghc.mkClsOcc "Succeed")
  showWrapperTyCon <- Plugin.tcLookupTyCon =<< Plugin.lookupOrig breakpointMod (Ghc.mkClsOcc "ShowWrapper")

  pure MkTcPluginNames{..}

data FindWantedResult
  = FoundLifted Ghc.Type Ghc.Ct
  | FoundUnlifted Ghc.Type Ghc.Ct
  | NotFound

findShowLevWanted
  :: TcPluginNames
  -> Ghc.Ct
  -> FindWantedResult
findShowLevWanted names ct
  | Ghc.CDictCan' _ di_cls di_tys <- ct
  , showLevClassName names == Ghc.getName di_cls
  , [Ghc.TyConApp tyCon [], arg2] <- di_tys
  = if Ghc.getName tyCon == Ghc.liftedRepName
       then FoundLifted arg2 ct
       else FoundUnlifted arg2 ct
  | otherwise = NotFound

findShowWithSuperclass
  :: TcPluginNames
  -> Ghc.Ct
  -> Maybe (Ghc.Type, Ghc.Ct)
findShowWithSuperclass names ct
  | Ghc.CDictCan' di_ev di_cls di_tys <- ct
  , Ghc.getName (showClass names) == Ghc.getName di_cls
  , hasShowLevSuperclass . Ghc.ctLocOrigin $ Ghc.ctev_loc di_ev
  , [arg] <- di_tys
  = Just (arg, ct)
  | otherwise = Nothing
  where
    hasShowLevSuperclass (Ghc.OccurrenceOf name)
      = name == showLevNameTc names
    hasShowLevSuperclass _ = False

solver :: TcPluginNames -> Ghc.TcPluginSolver
solver names _given _derived wanted = do
  instEnvs <- Plugin.getInstEnvs

  -- Check if wanted is ShowLev
  --   * Create a new wanted for Show
  --   * Use its EvBindId as the inner dict for ShowLev
  --   * Emit the new wanted
  (showLevDicts, mNewWanteds) <- fmap (unzip . catMaybes) $
    for (findShowLevWanted names <$> wanted) $ \case
      FoundUnlifted ty ct -> do
        unshowableDict <- Ghc.unsafeTcPluginTcM $ buildUnshowableDict ty
        pure $ Just ((unshowableDict, ct), Nothing)
      FoundLifted ty ct -> do
        (showDict, newWanted) <- buildShowLevDict names ct ty
        let (succInst, _) = fromRight (error "impossible: no Succeed instance") $
              Ghc.lookupUniqueInstEnv instEnvs (succeedClass names) [ty]
        pure $ Just
          ((liftDict succInst ty (getEvExprFromDict showDict), ct)
          , Just newWanted
          )
      NotFound -> pure Nothing

  -- Check if wanted is Show that arises from a use of showLev and create the
  -- missing Show dict if so.
  unshowableDicts <- for (findShowWithSuperclass names `mapMaybe` wanted) $
    \(ty, ct) -> do
        dict <- lookupUnshowableDict names ty
        pure (dict, ct)

  pure $ Ghc.TcPluginOk
           (showLevDicts ++ unshowableDicts)
           (catMaybes mNewWanteds)

buildShowLevDict
  :: TcPluginNames
  -> Ghc.Ct
  -> Ghc.Type
  -> Ghc.TcPluginM (Ghc.EvTerm, Ghc.Ct)
buildShowLevDict names showLevWanted ty = do
  showWantedEv <-
    Plugin.newWanted
      (Ghc.ctLoc showLevWanted)
      (Ghc.mkTyConApp (Ghc.classTyCon $ showClass names) [ty])
  let showCt = Ghc.mkNonCanonical showWantedEv
  pure (Ghc.ctEvTerm showWantedEv, showCt)

lookupUnshowableDict
  :: TcPluginNames
  -> Ghc.Type
  -> Ghc.TcPluginM Ghc.EvTerm
lookupUnshowableDict names ty = do
  instEnvs <- Plugin.getInstEnvs
  unshowableDict <- Ghc.unsafeTcPluginTcM $ buildUnshowableDict ty
  let (inst, _) = fromRight (error "impossible: no Show instance for ShowWrapper") $
        Ghc.lookupUniqueInstEnv
          instEnvs
          (showClass names)
          [Ghc.mkTyConApp (showWrapperTyCon names) [ty]]
  pure $ liftDict inst ty (getEvExprFromDict unshowableDict)

getEvExprFromDict :: Ghc.EvTerm -> Ghc.EvExpr
getEvExprFromDict = \case
  Ghc.EvExpr expr -> expr
  _ -> error "invalid argument to getEvExprFromDict"

buildUnshowableDict :: Ghc.Type -> Ghc.TcM Ghc.EvTerm
buildUnshowableDict ty = do
  let tyString = Ghc.showSDocOneLine' $ Ghc.pprTypeForUser' ty
  str <- Ghc.mkStringExpr $ "<" <> tyString <> ">"
  pure . Ghc.EvExpr $
    Ghc.mkCoreLams [Ghc.mkWildValBinder' ty] str

liftDict :: Ghc.ClsInst -> Ghc.Type -> Ghc.EvExpr -> Ghc.EvTerm
liftDict succ_inst ty dict = Ghc.evDFunApp (Ghc.is_dfun succ_inst) [ty] [dict]
