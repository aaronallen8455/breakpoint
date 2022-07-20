{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fplugin Debug.BreakPoint #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.QSem
import           System.IO.Unsafe
import           GHC.Exts

import Debug.BreakPoint

main :: IO ()
main = pure ()

data F = F

test :: String
test = showLev (Just id)
