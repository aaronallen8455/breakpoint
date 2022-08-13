{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}
module Main where

import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.QSem
import           System.IO.Unsafe
import           GHC.Exts hiding (breakpoint)

import Debug.Breakpoint
import Debug.Trace

main :: IO ()
main = test

test :: IO ()
test = do
  let w = 5# :: Int#
      x = "one"
      y = 2 :: Int
      zzzz = id :: Bool -> Bool
  breakpointIOP
  x <- getLine
  breakpointIOP
  pure ()
