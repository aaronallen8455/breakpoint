{-# LANGUAGE BangPatterns #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.QSem
import           System.IO.Unsafe

import qualified MyLib (someFunc)

{-# NOINLINE global  #-}
global :: QSem
global = unsafePerformIO $ newQSem 1

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  -- forkIO $ threadDelay 5000000 >> signalQSem global
  -- waitQSem global
  putStrLn breakMe
  forkIO $ putStrLn breakMe
  MyLib.someFunc
  threadDelay 50000000

{-# NOINLINE breakMe #-}
breakMe :: String
breakMe =
  let !_ = unsafePerformIO $ waitQSem global >> getLine >> signalQSem global
   in "test"

breakMe' :: String
breakMe' =
  let !_ = unsafePerformIO $ waitQSem global >> getLine >> signalQSem global
   in "test"

-- have a magic function named somthing like showVars :: String
-- That can be used with Debug.Trace or some other means of printing.
-- The parsed HsExpr has this function replaced by a string constructed by
-- showing all in scope variables (use well-typed lib for showing anything?
-- does it work for unlifted types? Can't find it, write my own?)
--
-- lib name ideas:
-- var-dump
