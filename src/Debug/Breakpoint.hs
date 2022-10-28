{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE ImplicitParams #-}
module Debug.Breakpoint
  ( -- * Plugin
    plugin
    -- * API
  , breakpoint
  , breakpointM
  , breakpointIO
  , queryVars
  , queryVarsM
  , queryVarsIO
    -- * Internals
  , captureVars
  , showLev
  , fromAscList
  , printAndWait
  , printAndWaitM
  , printAndWaitIO
  , runPrompt
  , runPromptM
  , runPromptIO
  , getSrcLoc
  ) where

import           Control.Applicative ((<|>), empty)
import           Control.Arrow ((&&&))
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Writer.CPS
import           Data.Char (isSpace)
import           Data.Data hiding (IntRep, FloatRep)
import           Data.Either
import           Data.Foldable
import           Data.Functor
import qualified Data.Graph as Graph
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import           Data.Maybe
import           Data.Monoid (Any(..))
import qualified Data.Text.Lazy as T
import           Data.Traversable (for)
import           Debug.Trace (trace, traceIO, traceM)
import qualified GHC.Exts as Exts
import           GHC.Int
#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Tc.Plugin as Plugin
#else
import qualified TcPluginM as Plugin
#endif
import           GHC.Word
import qualified System.Console.ANSI as ANSI
import qualified System.Console.Haskeline as HL
import           System.Environment (lookupEnv)
import           System.IO (stdout)
import           System.IO.Unsafe (unsafePerformIO)
import qualified Text.Pretty.Simple as PS
import qualified Text.Pretty.Simple.Internal.Color as PS

import qualified Debug.Breakpoint.GhcFacade as Ghc
import qualified Debug.Breakpoint.TimerManager as TM

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

-- | Constructs a lazy 'Map' from the names of all visible variables at the call
-- site to a string representation of their value. Does not include any variables
-- whose definitions contain it. Be careful not to assign multiple variables to
-- `captureVars` in the same scope as this will result in an infinite recursion.
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
  useColor <- ANSI.hSupportsANSIColor stdout
  let ?useColor = useColor
  prettyPrint <- usePrettyPrinting
  let ?prettyPrint = prettyPrint
  TM.suspendTimeouts $ do
    traceIO $ L.intercalate "\n"
      [ color red "### Breakpoint Hit ###"
      , color grey "(" <> srcLoc <> ")"
      , printVars vars
      , color green "Press enter to continue"
      ]
    void blockOnInput

runPrompt :: String -> M.Map String String -> a -> a
runPrompt srcLoc vars x =
  unsafePerformIO $ runPromptIO srcLoc vars >> pure x
{-# NOINLINE runPrompt #-}

runPromptM :: Applicative m => String -> M.Map String String -> m ()
runPromptM srcLoc vars = runPrompt srcLoc vars $ pure ()

runPromptIO :: forall m. MonadIO m => String -> M.Map String String -> m ()
runPromptIO srcLoc vars = liftIO . HL.runInputTBehavior HL.defaultBehavior settings $ do
    useColor <- liftIO $ ANSI.hSupportsANSIColor stdout
    let ?useColor = useColor
    prettyPrint <- liftIO usePrettyPrinting
    let ?prettyPrint = prettyPrint
    let printVar var val =
          HL.outputStrLn $ color cyan (var ++ " =\n") ++ prettify val
        inputLoop = do
          mInp <- HL.getInputLine $ color green "Enter variable name: "
          case mInp of
            Just (L.dropWhileEnd isSpace . dropWhile isSpace -> inp)
              | not (null inp) -> do
                  traverse_ (printVar inp) $ M.lookup inp vars
                  inputLoop
            _ -> pure ()
    HL.outputStrLn . unlines $
      [ color red "### Breakpoint Hit ###"
      , color grey $ "(" <> srcLoc <> ")"
      ] ++ (color cyan <$> varNames)
    inputLoop
  where
    settings = HL.setComplete completion HL.defaultSettings
    completion = HL.completeWord' Nothing isSpace $ \str ->
      pure $ HL.simpleCompletion
        <$> filter (str `L.isPrefixOf`) varNames
    varNames = M.keys vars

usePrettyPrinting :: IO Bool
usePrettyPrinting = isNothing <$> lookupEnv "NO_PRETTY_PRINT"

color :: (?useColor :: Bool) => String -> String -> String
color c s =
  if ?useColor
     then "\ESC[" <> c <> "m\STX" <> s <> "\ESC[m\STX"
     else s

red, green, grey, cyan :: String
red = "31"
green = "32"
grey = "37"
cyan = "36"

printVars :: (?useColor :: Bool, ?prettyPrint :: Bool)
          => M.Map String String -> String
printVars vars =
  let eqSign | ?prettyPrint = " =\n"
             | otherwise = " = "
      mkLine (k, v) = color cyan (k <> eqSign) <> prettify v
   in unlines . L.intersperse "" $ mkLine <$> M.toList vars

-- TODO don't apply parsing to things inside angle brackets
prettify :: (?prettyPrint :: Bool) => String -> String
prettify =
  if ?prettyPrint
  then T.unpack
     . PS.pStringOpt
         PS.defaultOutputOptionsDarkBg
           { PS.outputOptionsInitialIndent = 2
           , PS.outputOptionsIndentAmount = 2
           , PS.outputOptionsColorOptions = Just PS.ColorOptions
             { PS.colorQuote = PS.colorNull
             , PS.colorString = PS.colorBold PS.Vivid PS.Blue
             , PS.colorError = PS.colorBold PS.Vivid PS.Red
             , PS.colorNum = PS.colorBold PS.Vivid PS.Green
             , PS.colorRainbowParens = [PS.colorBold PS.Vivid PS.Cyan]
             }
           }
  else id

inactivePluginStr :: String
inactivePluginStr =
  "Cannot set breakpoint: the Debug.Trace plugin is not active"

-- | Sets a breakpoint in pure code
breakpoint :: a -> a
breakpoint = trace inactivePluginStr

-- | When evaluated, displays the names of variables visible from the callsite
-- and starts a prompt where entering a variable will display its value. You
-- may want to use this instead of 'breakpoint' if there are value which should
-- stay unevaluated or you are only interested in certain values. Only the
-- current thread is blocked while the prompt is active. To resume execution,
-- press enter with a blank prompt.
queryVars :: a -> a
queryVars = trace inactivePluginStr

-- | Similar to 'queryVars' but for use in an arbitrary 'Applicative' context.
-- This uses 'unsafePerformIO' which means that laziness and common sub-expression
-- elimination can result in unexpected behavior. For this reason you should
-- prefer 'queryVarsIO' if a 'MonadIO' instance is available.
queryVarsM :: Applicative m => m ()
queryVarsM = traceM inactivePluginStr

-- | Similar to 'queryVars' but specialized to an 'IO' context. You should favor
-- this over 'queryVarsM' if a 'MonadIO' instance is available.
queryVarsIO :: MonadIO m => m ()
queryVarsIO =
  liftIO (traceIO inactivePluginStr)

-- | Sets a breakpoint in an arbitrary 'Applicative'. Uses 'unsafePerformIO'
-- which means that laziness and common sub-expression elimination can result
-- in the breakpoint not being hit as expected. For this reason, you should
-- prefer 'breakpointIO' if a `MonadIO` instance is available.
breakpointM :: Applicative m => m ()
breakpointM = traceM inactivePluginStr

-- | Sets a breakpoint in an 'IO' based 'Monad'. You should favor this over
-- 'breakpointM' if the monad can perform IO.
breakpointIO :: MonadIO m => m ()
breakpointIO =
  liftIO (traceIO inactivePluginStr)

-- | Pretty prints the source code location of its call site
getSrcLoc :: String
getSrcLoc = ""

#if MIN_VERSION_ghc(9,2,0)
-- Use an "unsafe" foreign function to more or less stop the runtime.
-- In older GHCs this can cause out of control CPU usage so settle for getLine instead
foreign import ccall unsafe "stdio.h getchar" blockOnInput :: IO Int
#else
blockOnInput :: IO Int
blockOnInput = 1 <$ getLine
#endif

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
        . Ghc.showSDocUnsafe
        . Ghc.ppr
        $ Ghc.locA' loc

      captureVarsExpr =
        let mkTuple (Ghc.fromLexicalFastString -> varStr, n) =
              Ghc.mkLHsTupleExpr
                [ Ghc.nlHsLit . Ghc.mkHsString $ Ghc.unpackFS varStr
                , Ghc.nlHsApp (Ghc.nlHsVar showLevName) (Ghc.nlHsVar n)
                ]
#if MIN_VERSION_ghc(9,2,0)
                Ghc.NoExtField
#endif

            mkList exprs = Ghc.noLocA' (Ghc.ExplicitList' Ghc.NoExtField exprs)

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

      queryVarsIOExpr =
        Ghc.nlHsApp
          (Ghc.nlHsApp (Ghc.nlHsVar runPromptIOName) srcLocStringExpr)
          captureVarsExpr

      queryVarsExpr =
        Ghc.nlHsApp
          (Ghc.nlHsApp (Ghc.nlHsVar runPromptName) srcLocStringExpr)
          captureVarsExpr

      queryVarsMExpr =
        Ghc.nlHsApp
          (Ghc.nlHsApp (Ghc.nlHsVar runPromptMName) srcLocStringExpr)
          captureVarsExpr

  if | captureVarsName == name -> do
         tell $ Any True
         pure (Just $ Ghc.unLoc captureVarsExpr)

     | breakpointName == name -> do
         tell $ Any True
         pure (Just $ Ghc.unLoc bpExpr)

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
         pure (Just $ Ghc.unLoc queryVarsExpr)

     | queryVarsMName == name -> do
         tell $ Any True
         pure (Just $ Ghc.unLoc queryVarsMExpr)

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
#if !MIN_VERSION_ghc(9,0,0)
matchCase _ = pure Nothing
#endif

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
#if MIN_VERSION_ghc(9,2,0)
         grhssLocalBinds
#else
         (Ghc.unLoc grhssLocalBinds)
#endif

  grhsRes <- addScopedVars names $ recurse grhssGRHSs
  pure $ Just
    Ghc.GRHSs { Ghc.grhssGRHSs = grhsRes
#if MIN_VERSION_ghc(9,2,0)
              , grhssLocalBinds = localBindsRes
#else
              , grhssLocalBinds = localBindsRes <$ grhssLocalBinds
#endif
              , ..
              }
#if !MIN_VERSION_ghc(9,0,0)
grhssCase _ = pure Nothing
#endif

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
#if !MIN_VERSION_ghc(9,0,0)
grhsCase _ = pure Nothing
#endif

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
  Ghc.BindStmt' x lpat body bindExpr failExpr -> do
    let names = extractVarPats lpat
    tell names
    bodyRes <- lift $ recurse body
    pure $ Ghc.BindStmt' x lpat bodyRes bindExpr failExpr

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
#if !MIN_VERSION_ghc(9,0,0)
          a -> lift $ gmapM recurse a
#endif
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
#if !MIN_VERSION_ghc(9,0,0)
      _ -> empty
#endif
    pure $ Ghc.HsProc x1 lpat cmdTopRes
hsProcCase _ = pure Nothing

--------------------------------------------------------------------------------
-- Env
--------------------------------------------------------------------------------

-- The writer is for tracking if an inner expression contains the target name
type EnvReader = WriterT Any (Reader Env)

type VarSet = M.Map Ghc.LexicalFastString' Ghc.Name

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
  }

overVarSet :: (VarSet -> VarSet) -> Env -> Env
overVarSet f env = env { varSet = f $ varSet env }

getOccNameFS :: Ghc.Name -> Ghc.LexicalFastString'
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
  = Just $ if Ghc.getName tyCon == Ghc.liftedRepName
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
  case Ghc.lookupUniqueInstEnv instEnvs cls tys of
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
                  (showClass names)
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
  let tyString = Ghc.showSDocOneLine' $ Ghc.pprTypeForUser' ty
  str <- Ghc.mkStringExpr $ "<" <> tyString <> ">"
  pure . Ghc.EvExpr $
    Ghc.mkCoreLams [Ghc.mkWildValBinder' ty] str

liftDict :: Ghc.ClsInst -> Ghc.Type -> Ghc.EvExpr -> Ghc.EvTerm
liftDict succ_inst ty dict = Ghc.evDFunApp (Ghc.is_dfun succ_inst) [ty] [dict]

--------------------------------------------------------------------------------
-- Showing
--------------------------------------------------------------------------------

-- | Levity polymorphic 'Show'
class ShowLev (rep :: Exts.RuntimeRep) (a :: Exts.TYPE rep) where
  showLev :: a -> String

instance ShowLev 'Exts.IntRep Exts.Int# where
  showLev i = show $ I# i

#if MIN_VERSION_base(4,16,0)
instance ShowLev 'Exts.Int8Rep Exts.Int8# where
  showLev i = show $ I8# i

instance ShowLev 'Exts.Int16Rep Exts.Int16# where
  showLev i = show $ I16# i

instance ShowLev 'Exts.Int32Rep Exts.Int32# where
  showLev i = show $ I32# i
#endif

#if MIN_VERSION_base(4,17,0)
instance ShowLev 'Exts.Int64Rep Exts.Int64# where
  showLev i = show $ I64# i
#endif

instance ShowLev 'Exts.WordRep Exts.Word# where
  showLev w = show $ W# w

#if MIN_VERSION_base(4,16,0)
instance ShowLev 'Exts.Word8Rep Exts.Word8# where
  showLev w = show $ W8# w

instance ShowLev 'Exts.Word16Rep Exts.Word16# where
  showLev w = show $ W16# w

instance ShowLev 'Exts.Word32Rep Exts.Word32# where
  showLev w = show $ W32# w
#endif

#if MIN_VERSION_base(4,17,0)
instance ShowLev 'Exts.Word64Rep Exts.Word64# where
  showLev w = show $ W64# w
#endif

instance ShowLev 'Exts.FloatRep Exts.Float# where
  showLev f = show $ Exts.F# f

instance ShowLev 'Exts.DoubleRep Exts.Double# where
  showLev d = show $ Exts.D# d

newtype ShowWrapper a = MkShowWrapper a

instance ShowLev Exts.LiftedRep a => Show (ShowWrapper a) where
  show (MkShowWrapper a) = showLev a

class Succeed a where
  _succeed :: a -> String

-- Looking up an instance of this class for any type will always succeed. To
-- produce actual evidence, a Show dict must be provided.
instance Show a => Succeed a where
  _succeed = show
