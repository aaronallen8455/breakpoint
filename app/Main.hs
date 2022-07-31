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
  let w = 5# :: Int#
      x = "one"
      y = 2 :: Int
      z = id :: Bool -> Bool
  putStrLn "start"
  forkIO $ do
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
