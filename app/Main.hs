{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.QSem
import           System.IO.Unsafe
import           GHC.Exts

import Debug.Breakpoint
import Debug.Trace

main :: IO ()
main = pure ()

data F = F

test :: IO ()
test = do
  let w = 5# :: Int#
      x = "one"
      y = 2 :: Int
      z = id :: Bool -> Bool
  forkIO $ do
    threadDelay 1000000
    putStrLn "..."
    threadDelay 1000000
    putStrLn "..."
  breakpointIO
  x <- getLine
  breakpointIO
  pure ()
