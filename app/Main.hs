{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fplugin Debug.BreakPoint #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.QSem
import           System.IO.Unsafe
import           GHC.Exts

import Debug.BreakPoint
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
  bpIO
  x <- getLine
  bpIO
  pure ()
