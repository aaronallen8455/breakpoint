{-# LANGUAGE DataKinds #-} -- for 9.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , excludeVars
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

import           Control.Monad.IO.Class
import           Data.Char (isSpace)
import           Data.Foldable
import           Data.Functor
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import           Data.Maybe
import qualified Data.Text.Lazy as T
import           Debug.Trace (trace, traceIO, traceM)
import qualified GHC.Exts as Exts
import           GHC.Int
import           GHC.Word
import qualified System.Console.ANSI as ANSI
import qualified System.Console.Haskeline as HL
import           System.Environment (lookupEnv)
import           System.IO (stdout)
import           System.IO.Unsafe (unsafePerformIO)
import qualified Text.Pretty.Simple as PS
import qualified Text.Pretty.Simple.Internal.Color as PS

import qualified Debug.Breakpoint.GhcFacade as Ghc
import qualified Debug.Breakpoint.Renamer as Renamer
import qualified Debug.Breakpoint.TimerManager as TM
import qualified Debug.Breakpoint.TypeChecker as TypeChecker

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

-- | Excludes the given variable names from appearing in the output of any
-- breakpoints occurring in the given expression.
excludeVars :: [String] -> a -> a
excludeVars _ = id

--------------------------------------------------------------------------------
-- Plugin
--------------------------------------------------------------------------------

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.renamedResultAction = const Renamer.renameAction
  , Ghc.tcPlugin = const $ Just TypeChecker.tcPlugin
  }

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
