{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Arrows #-}
import           Control.Arrow
import           Control.Monad
import           Data.Fixed
import qualified Data.Map as M
import           Data.Maybe
import           GHC.Records (HasField(..))
import           Test.Tasty
import           Test.Tasty.HUnit

import           Debug.Breakpoint
import qualified ApplicativeDo as ApDo
import qualified OverloadedStrings as OS

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup "Tests"
    [ testCase "function args" functionArgs
    , testCase "where binds" whereBinds
    , testCase "args and where" argsAndWhere
    , testCase "let bindings" letBindings
    , testCase "nested in let" nestedInLet
    , testCase "let scoping" letScoping
    , testCase "nested in where" nestedInWhere
    , testCase "guard binding" guardBinding
    , testCase "pattern guard" patternGuard
    , testCase "lambda bind" lambdaBind
    , testCase "no let escape" noLetEscape
    , testCase "case pattern bind" casePatBind
    , testCase "monadic binds" monadicBinds
    , testCase "monadic binds scoped" monadicBindsScoped
    , testCase "do block let bind" doBlockLetBind
    , testCase "list comprehension" listComprehension
    , testCase "arrow notation" arrowNotation
    , testCase "record field bindings" recFieldBindings
    , testCase "record wild cards" recWildCards
    , testCase "do block in where bind" doBlockInWhereBind
    , testCase "don't capture do bind in its body" captureInBodyOfDoBind
    , testCase "Shows type that subclass for Show" showFixedPointNumber
    , testCase "exclude vars" excludeVarsTest
    , testCase "built-in instance subclasses" showBuiltinSubclass
    , ApDo.testTree
    , OS.testTree
    ]
    -- TODO
    -- Implicit Params
    -- Pattern synonyms
    -- recursive do
    -- qualified do

functionArgs :: Assertion
functionArgs = test1 1 True @?= M.fromList [("b", "True"), ("i", "1")]

test1 :: Int -> Bool -> M.Map String String
test1 i b = captureVars

whereBinds :: Assertion
whereBinds = test2 @?= M.fromList [("a", "1"), ("b", "True")]

test2 :: M.Map String String
test2 = captureVars
  where a = 1 :: Int
        b = True

argsAndWhere :: Assertion
argsAndWhere = test3 1 @?= M.fromList [("a", "1"), ("b", "2"), ("c", "3")]

test3 :: Int -> M.Map String String
test3 a = captureVars
  where b = 2 :: Int
        c = 3 :: Int

letBindings :: Assertion
letBindings =
  test4 @?= M.fromList [("a", "True"), ("b", "1")]

test4 :: M.Map String String
test4 =
  let a = True
      b = 1 :: Int
   in captureVars

nestedInLet :: Assertion
nestedInLet = test5 1 @?= M.fromList [("a", "1"), ("b", "2"), ("c", "3")]

test5 :: Int -> M.Map String String
test5 a =
  let x =
        let b = 2 :: Int
            c = 3 :: Int
         in captureVars
   in x

noLetEscape :: Assertion
noLetEscape = test6 @?= M.fromList [("a", "()")]

test6 :: M.Map String String
test6 =
  let a =
        let b = True
            c = False
         in ()
   in captureVars

nestedInWhere :: Assertion
nestedInWhere = test7 @?= M.fromList [("a", "()")]

test7 :: M.Map String String
test7 = captureVars where
  a = () where
          b = True
          c = False

guardBinding :: Assertion
guardBinding = test8 @?= M.fromList [("a", "()"), ("b", "()")]

test8 :: M.Map String String
test8 | let a = (), let b = a = captureVars

patternGuard :: Assertion
patternGuard = test9 @?= M.fromList [("a", "()")]

test9 :: M.Map String String
test9 | Just a <- Just () = captureVars

lambdaBind :: Assertion
lambdaBind = test10 () @?= M.fromList [("a", "()")]

test10 :: () -> M.Map String String
test10 = \a -> captureVars

letScoping :: Assertion
letScoping = test11 @?= M.fromList [("b", "True"), ("c", "False")]

test11 :: M.Map String String
test11 =
  let b = True
      a = captureVars
      c = False
   in a

casePatBind :: Assertion
casePatBind = test12 @?= M.fromList [("a", "()")]

test12 :: M.Map String String
test12 = case Just () of
           Just a -> captureVars

monadicBinds :: Assertion
monadicBinds = test13 @?= M.fromList [("a", "True"), ("b", "False"), ("x", "5")]

test13 :: M.Map String String
test13 = fromMaybe mempty $ do
  a <- Just True
  b <- Just False
  let x = 5 :: Int
  pure captureVars

monadicBindsScoped :: Assertion
monadicBindsScoped = test14 @?= M.fromList [("a", "True")]

test14 :: M.Map String String
test14 = fromMaybe mempty $ do
  a <- Just True
  let m = captureVars
  b <- Just False
  pure m

doBlockLetBind :: Assertion
doBlockLetBind = test15 @?= M.fromList [("a", "True"), ("b", "False")]

test15 :: M.Map String String
test15 = fromMaybe mempty $ do
  let a = True
  b <- Just False
  pure captureVars

listComprehension :: Assertion
listComprehension = test16 @?= M.fromList [("a", "True"), ("b", "False")]

test16 = head [ captureVars | let b = False, a <- [True] ]

arrowNotation :: Assertion
arrowNotation = test17 @?= M.fromList [("a", "2"), ("b", "0"), ("x", "1")]

test17 :: M.Map String String
test17 = go (1 :: Int) where
  go = proc x -> do
    a <- succ -< (x :: Int)
    let b = pred x
    returnA -< captureVars

recFieldBindings :: Assertion
recFieldBindings = test18 MkRec {fld=True} @?= M.fromList [("a", "True")]

data Rec = MkRec { fld :: Bool }
test18 :: Rec -> M.Map String String
test18 MkRec { fld = a } = captureVars

recWildCards :: Assertion
recWildCards = test19 MkRec {fld=True} @?= M.fromList [("fld", "True")]

test19 :: Rec -> M.Map String String
test19 MkRec{..} = captureVars

doBlockInWhereBind :: Assertion
doBlockInWhereBind = do
  r <- test20
  M.delete "whereDo" r @?= M.fromList [("x", "True")]

test20 :: forall m. Monad m => m (M.Map String String)
test20 = do
  x <- whereDo True
  pure captureVars
  where
    whereDo :: Bool -> m Bool
    whereDo y = do
      pure y

captureInBodyOfDoBind :: Assertion
captureInBodyOfDoBind = do
  r <- test21
  M.delete "wb" r @?= M.fromList [("y", "True")]

test21 :: IO (M.Map String String)
test21 = do
  x <- wb $ \y -> do
    pure captureVars
  pure x
  where
    wb k = k True

showFixedPointNumber :: Assertion
showFixedPointNumber = do
  let m = test22
  m @?= M.fromList [("x", "4.000000")]

test22 :: M.Map String String
test22 = let x = (4 :: Micro) in captureVars

excludeVarsTest :: Assertion
excludeVarsTest = do
  let m = test23
  m @?= M.fromList [("x", "True")]

test23 :: M.Map String String
test23 =
  let x = True
      y = False
   in excludeVars ["y"] captureVars

showBuiltinSubclass :: Assertion
showBuiltinSubclass = do
  let m = test24
  m @?= M.fromList [("x", "1"), ("y", "91.00")]

test24 :: M.Map String String
test24 =
  let x = HF $ FFF 1
      y = 91 :: Fixed 91
   in captureVars

newtype HF a = HF a
instance (HasField "fff" a b, Show b) => Show (HF a) where
  show (HF a) = show $ getField @"fff" a

newtype FFF = FFF { fff :: Int }
