{-# OPTIONS_GHC -fplugin Debug.BreakPoint #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.QSem
import           System.IO.Unsafe

main :: IO ()
main = pure ()

data F = F

test :: String
test = show (Just F)
