{-# LANGUAGE GADTs #-} -- for 8.10
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Debug.Breakpoint.Renamer
  ( renameAction
  ) where

import           Control.Applicative ((<|>), empty)
import           Control.Arrow ((&&&))
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Writer.CPS
import           Data.Data hiding (IntRep, FloatRep)
import qualified Data.Graph as Graph
import qualified Data.Map.Lazy as M
import           Data.Maybe
import           Data.Monoid (Any(..))
import           Data.Traversable (for)

import qualified Debug.Breakpoint.GhcFacade as Ghc

renameAction
  :: Ghc.TcGblEnv
  -> Ghc.HsGroup Ghc.GhcRn
  -> Ghc.TcM (Ghc.TcGblEnv, Ghc.HsGroup Ghc.GhcRn)
renameAction gblEnv group = do
  Ghc.Found _ breakpointMod <-
    Ghc.findPluginModule' (Ghc.mkModuleName "Debug.Breakpoint")

  captureVarsName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "captureVars")
  showLevName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "showLev")
  fromListName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "fromAscList")
  breakpointName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "breakpoint")
  queryVarsName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "queryVars")
  breakpointMName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "breakpointM")
  queryVarsMName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "queryVarsM")
  breakpointIOName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "breakpointIO")
  queryVarsIOName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "queryVarsIO")
  printAndWaitName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "printAndWait")
  printAndWaitMName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "printAndWaitM")
  printAndWaitIOName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "printAndWaitIO")
  runPromptIOName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "runPromptIO")
  runPromptMName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "runPromptM")
  runPromptName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "runPrompt")
  getSrcLocName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "getSrcLoc")
  excludeVarsName <- Ghc.lookupOrig breakpointMod (Ghc.mkVarOcc "excludeVars")

  (group', _) <-
    runReaderT (runWriterT $ recurse group)
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
    <|> wrap hsAppCase
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
        . Ghc.showSDocUnsafe
        . Ghc.ppr
        $ Ghc.locA loc

      captureVarsExpr mResultName =
        let mkTuple (Ghc.fromLexicalFastString -> varStr, n) =
              Ghc.mkLHsTupleExpr
                [ Ghc.nlHsLit . Ghc.mkHsString $ Ghc.unpackFS varStr
                , Ghc.nlHsApp (Ghc.nlHsVar showLevName) (Ghc.nlHsVar n)
                ]
                Ghc.NoExtField

            mkList exprs = Ghc.noLocA (Ghc.ExplicitList Ghc.NoExtField exprs)

            varSetWithResult
              | Just resName <- mResultName =
                  M.insert (Ghc.mkLexicalFastString $ Ghc.mkFastString "*result")
                           resName
                           varSet
              | otherwise = varSet

         in Ghc.nlHsApp (Ghc.nlHsVar fromListName) . mkList
              $ mkTuple <$> M.toList varSetWithResult

      bpExpr = do
        resultName <- Ghc.newName (Ghc.mkOccName Ghc.varName "_result_")
        pure $
          Ghc.mkHsLam [Ghc.nlVarPat resultName] $
            Ghc.nlHsApp
              (Ghc.nlHsApp
                (Ghc.nlHsApp (Ghc.nlHsVar printAndWaitName) srcLocStringExpr)
                (captureVarsExpr $ Just resultName)
              )
              (Ghc.nlHsVar resultName)

      bpMExpr =
        Ghc.nlHsApp
          (Ghc.nlHsApp (Ghc.nlHsVar printAndWaitMName) srcLocStringExpr)
          $ captureVarsExpr Nothing

      bpIOExpr =
        Ghc.nlHsApp
          (Ghc.nlHsApp (Ghc.nlHsVar printAndWaitIOName) srcLocStringExpr)
          $ captureVarsExpr Nothing

      queryVarsIOExpr =
        Ghc.nlHsApp
          (Ghc.nlHsApp (Ghc.nlHsVar runPromptIOName) srcLocStringExpr)
          $ captureVarsExpr Nothing

      queryVarsExpr = do
        resultName <- Ghc.newName (Ghc.mkOccName Ghc.varName "_result_")
        pure $
          Ghc.mkHsLam [Ghc.nlVarPat resultName] $
            Ghc.nlHsApp
              (Ghc.nlHsApp
                (Ghc.nlHsApp (Ghc.nlHsVar runPromptName) srcLocStringExpr)
                (captureVarsExpr $ Just resultName)
              )
              (Ghc.nlHsVar resultName)

      queryVarsMExpr =
        Ghc.nlHsApp
          (Ghc.nlHsApp (Ghc.nlHsVar runPromptMName) srcLocStringExpr)
          $ captureVarsExpr Nothing

  if | captureVarsName == name -> do
         tell $ Any True
         pure (Just . Ghc.unLoc $ captureVarsExpr Nothing)

     | breakpointName == name -> do
         tell $ Any True
         Just . Ghc.unLoc <$> lift (lift bpExpr)

     | breakpointMName == name -> do
         tell $ Any True
         pure (Just $ Ghc.unLoc bpMExpr)

     | breakpointIOName == name -> do
         tell $ Any True
         pure (Just $ Ghc.unLoc bpIOExpr)

     | queryVarsIOName == name -> do
         tell $ Any True
         pure (Just $ Ghc.unLoc queryVarsIOExpr)

     | queryVarsName == name -> do
         tell $ Any True
         Just . Ghc.unLoc <$> lift (lift queryVarsExpr)

     | queryVarsMName == name -> do
         tell $ Any True
         pure (Just $ Ghc.unLoc queryVarsMExpr)

     | getSrcLocName == name ->
         pure (Just $ Ghc.unLoc srcLocStringExpr)

     | otherwise -> pure Nothing
hsVarCase _ = pure Nothing

--------------------------------------------------------------------------------
-- App Expr
--------------------------------------------------------------------------------

hsAppCase :: Ghc.LHsExpr Ghc.GhcRn
          -> EnvReader (Maybe (Ghc.LHsExpr Ghc.GhcRn))
hsAppCase (Ghc.unLoc -> Ghc.HsApp _ f innerExpr)
  | Ghc.HsApp _ (Ghc.unLoc -> Ghc.HsVar _ (Ghc.unLoc -> name))
                (Ghc.unLoc -> Ghc.ExplicitList' _ exprsToExclude)
      <- Ghc.unLoc f
  = do
    MkEnv{..} <- lift ask
    if excludeVarsName /= name
       then pure Nothing
       else do
         let extractVarName (Ghc.HsLit _ (Ghc.HsString _ fs)) =
               Just $ Ghc.mkLexicalFastString fs
             extractVarName (Ghc.HsOverLit _ (Ghc.OverLit' (Ghc.HsIsString _ fs))) =
               Just $ Ghc.mkLexicalFastString fs
             extractVarName _ = Nothing

             varsToExclude =
               mapMaybe (extractVarName . Ghc.unLoc) exprsToExclude

         Just <$>
           mapWriterT
            (local (overVarSet $ \vs -> foldr M.delete vs varsToExclude))
            (recurse innerExpr)
hsAppCase _ = pure Nothing

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
extractVarPats = mkVarSet . Ghc.collectPatBinders'

--------------------------------------------------------------------------------
-- Guarded Right-hand Sides
--------------------------------------------------------------------------------

grhssCase :: Ghc.GRHSs Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
         -> EnvReader (Maybe (Ghc.GRHSs Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)))
grhssCase Ghc.GRHSs {..} = do
  (localBindsRes, names)
    <- dealWithLocalBinds
         grhssLocalBinds

  grhsRes <- addScopedVars names $ recurse grhssGRHSs
  pure $ Just
    Ghc.GRHSs { Ghc.grhssGRHSs = grhsRes
              , grhssLocalBinds = localBindsRes
              , ..
              }

dealWithBind :: VarSet
             -> Ghc.LHsBind Ghc.GhcRn
             -> EnvReader (Ghc.LHsBind Ghc.GhcRn)
dealWithBind resultNames lbind = for lbind $ \case
  Ghc.FunBind {..} -> do
    let resultNamesSansSelf =
          M.delete (getOccNameFS $ Ghc.unLoc fun_id) resultNames
    (matchesRes, Any containsTarget)
      <- listen
       . addScopedVars resultNamesSansSelf
       $ recurse fun_matches
    -- be sure to use the result names on the right so that they are overriden
    -- by any shadowing vars inside the expr.
    let rhsVars
          | containsTarget
          = Ghc.mkUniqSet . M.elems
            . (<> resultNamesSansSelf) . mkVarSet
            $ Ghc.nonDetEltsUniqSet fun_ext
          | otherwise = fun_ext
    pure Ghc.FunBind { Ghc.fun_matches = matchesRes, Ghc.fun_ext = rhsVars, .. }

  Ghc.PatBind {..} -> do
    (rhsRes, Any containsTarget)
      <- listen
       . addScopedVars resultNames
       $ recurse pat_rhs
    let rhsVars
          | containsTarget
          = Ghc.mkUniqSet . M.elems
            . (<> resultNames) . mkVarSet
            $ Ghc.nonDetEltsUniqSet pat_ext
          | otherwise = pat_ext
    pure Ghc.PatBind { Ghc.pat_rhs = rhsRes, pat_ext = rhsVars, .. }

  -- Does this not occur in the renamer?
  Ghc.VarBind {..} -> do
    rhsRes
      <- addScopedVars resultNames
       $ recurse var_rhs
    pure Ghc.VarBind { Ghc.var_rhs = rhsRes, .. }

  Ghc.PatSynBind x Ghc.PSB {..} -> do
    (defRes, Any containsTarget)
      <- listen
       . addScopedVars resultNames
       $ recurse psb_def
    let rhsVars
          | containsTarget
          = Ghc.mkUniqSet . M.elems
            . (<> resultNames) . mkVarSet
            $ Ghc.nonDetEltsUniqSet psb_ext
          | otherwise = psb_ext
    pure $ Ghc.PatSynBind x Ghc.PSB { psb_def = defRes, psb_ext = rhsVars, .. }

#if !MIN_VERSION_ghc(9,4,0)
  other -> pure other
#endif

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
hsLetCase (Ghc.HsLet' x letToken (Ghc.L loc localBinds) inToken inExpr) = do
  (bindsRes, names) <- dealWithLocalBinds localBinds

  inExprRes <- addScopedVars names $ recurse inExpr
  pure . Just $
    Ghc.HsLet' x letToken (Ghc.L loc bindsRes) inToken inExprRes
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
          names = map (foldMap Ghc.collectHsBindBinders')
                      binds
          resultNames = mkVarSet $ concat names

      (resBindsWithNames, Any containsTarget)
        <- listen
         . fmap (`zip` names)
         $ traverse (dealWithBind resultNames) binds

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
    bodyRes <- lift $ recurse body
    pure $ Ghc.BindStmt x lpat bodyRes

  Ghc.LetStmt' x (Ghc.L loc localBinds) -> do
    (bindsRes, names) <- lift $ dealWithLocalBinds localBinds
    tell names
    pure $ Ghc.LetStmt' x (Ghc.L loc bindsRes)

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
type EnvReader = WriterT Any (ReaderT Env Ghc.TcM)

type VarSet = M.Map Ghc.LexicalFastString Ghc.Name

data Env = MkEnv
  { varSet :: !VarSet
  , captureVarsName :: !Ghc.Name
  , showLevName :: !Ghc.Name
  , fromListName :: !Ghc.Name
  , breakpointName :: !Ghc.Name
  , queryVarsName :: !Ghc.Name
  , breakpointMName :: !Ghc.Name
  , queryVarsMName :: !Ghc.Name
  , breakpointIOName :: !Ghc.Name
  , queryVarsIOName :: !Ghc.Name
  , printAndWaitName :: !Ghc.Name
  , printAndWaitMName :: !Ghc.Name
  , printAndWaitIOName :: !Ghc.Name
  , runPromptIOName :: !Ghc.Name
  , runPromptName :: !Ghc.Name
  , runPromptMName :: !Ghc.Name
  , getSrcLocName :: !Ghc.Name
  , excludeVarsName :: !Ghc.Name
  }

overVarSet :: (VarSet -> VarSet) -> Env -> Env
overVarSet f env = env { varSet = f $ varSet env }

getOccNameFS :: Ghc.Name -> Ghc.LexicalFastString
getOccNameFS = Ghc.mkLexicalFastString . Ghc.occNameFS . Ghc.getOccName

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

