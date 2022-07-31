{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ApplicativeDo #-}
module ApplicativeDo
  ( testTree
  ) where

import qualified Data.Map as M
import           Data.Maybe
import           Test.Tasty
import           Test.Tasty.HUnit

import           Debug.Breakpoint

-- Needs to be a separate module b/c ApplicativeDo affects other tests

testTree :: TestTree
testTree = testGroup "ApplicativeDo"
  [ testCase "applicative do bindings" applicativeDoBindings
  , testCase "monadic binds scoped" monadicBindsScoped
  ]

applicativeDoBindings :: Assertion
applicativeDoBindings =
  runM test1 @?= Just (M.fromList [("a", "True"), ("b", "False")])

newtype M a = M { runM :: Maybe a }
  deriving newtype (Functor, Applicative)

test1 :: Applicative m => m (M.Map String String)
test1 = do
  let b = False
  a <- pure True
  return captureVars

monadicBindsScoped :: Assertion
monadicBindsScoped = M.delete "m" test2 @?= M.fromList [("a", "True")]

test2 :: M.Map String String
test2 = fromMaybe mempty $ do
  a <- Just True
  let m = const captureVars a -- NB: need to reference 'a' here b/c of ApplicativeDo
  b <- Just False
  pure m
