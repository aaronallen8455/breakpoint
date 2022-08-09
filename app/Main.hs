{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}
module Main where

import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.QSem
import           System.IO.Unsafe
import           GHC.Exts

import Debug.Breakpoint
import Debug.Trace

main :: IO ()
main = test

data F = F

test :: IO ()
test = do
--   putStrLn "1"
--   forkIO $ do
--     breakpointIO
--     putStrLn "3"
--   threadDelay 10000000
--   putStrLn "2"

  let w = 5# :: Int#
      x = "one"
      y = 2 :: Int
      z = id :: Bool -> Bool
      a = MkNoShow {f1 = True, f2 = 2}
  putStrLn "start"
  forkOS $ do
    threadDelay 1000000
    putStrLn "1"
    breakpointIO
    threadDelay 1000000
    putStrLn "2"
  breakpointIO
  putStrLn "3"
  threadDelay 1000000
  putStrLn "4"
  threadDelay 1000000
  putStrLn "5"
  x <- getLine
  breakpointIO
  pure ()

data NoShow =
  MkNoShow
    { f1 :: Bool
    , f2 :: Int
    }
