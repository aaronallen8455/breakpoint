import qualified Data.Map as M
import           Test.Tasty
import           Test.Tasty.HUnit

import           Debug.BreakPoint

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
    , testCase "no let escape" noLetEscape
    , testCase "nested in where" nestedInWhere
    , testCase "guard binding" guardBinding
    -- TODO
    -- things bound in guards
    -- monadic binds
    -- let bind in do
    -- lamda binds
    -- case branch binds
    ]

functionArgs :: Assertion
functionArgs =
  test1 1 True @?= M.fromList [("b", "True"), ("i", "1")]

test1 :: Int -> Bool -> M.Map String String
test1 i b = traceVars

whereBinds :: Assertion
whereBinds =
  test2 @?= M.fromList [("a", "1"), ("b", "True")]

test2 :: M.Map String String
test2 = traceVars
  where a = 1 :: Int
        b = True

argsAndWhere :: Assertion
argsAndWhere =
  test3 1 @?= M.fromList [("a", "1"), ("b", "2"), ("c", "3")]

test3 :: Int -> M.Map String String
test3 a = traceVars
  where b = 2 :: Int
        c = 3 :: Int

letBindings :: Assertion
letBindings =
  test4 @?= M.fromList [("a", "True"), ("b", "1")]

test4 :: M.Map String String
test4 =
  let a = True
      b = 1 :: Int
   in traceVars

nestedInLet :: Assertion
nestedInLet =
  test5 1 @?= M.fromList [("a", "1"), ("b", "2"), ("c", "3")]

test5 :: Int -> M.Map String String
test5 a =
  let x =
        let b = 2
            c = 3
         in traceVars
   in x

noLetEscape :: Assertion
noLetEscape =
  test6 @?= M.fromList [("a", "()")]

test6 :: M.Map String String
test6 =
  let a =
        let b = True
            c = False
         in ()
   in traceVars

nestedInWhere :: Assertion
nestedInWhere =
  test7 @?= M.fromList [("a", "()")]

test7 :: M.Map String String
test7 = traceVars where
  a = () where
          b = True
          c = False

guardBinding :: Assertion
guardBinding =
  test8 @?= M.fromList [("a", "()")]

test8 :: M.Map String String
test8 | let a = (), let b = a = traceVars
