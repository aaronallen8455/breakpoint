{-# LANGUAGE OverloadedStrings #-}
module OverloadedStrings
  ( testTree
  ) where

import qualified Data.Map as M
import           Test.Tasty
import           Test.Tasty.HUnit

import           Debug.Breakpoint

-- Needs to be a separate module b/c ApplicativeDo affects other tests

testTree :: TestTree
testTree = testGroup "IsString"
  [ testCase "exclude vars" excludeVarsTest
  ]

excludeVarsTest :: Assertion
excludeVarsTest = do
  let m = test23
  m @?= M.fromList [("x", "True")]

test23 :: M.Map String String
test23 =
  let x = True
      y = False
   in excludeVars ["y"] captureVars
