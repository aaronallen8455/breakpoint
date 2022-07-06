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
    -- TODO
    -- things bound in guards
    -- monadic binds
    -- let bind in do
    -- lamda binds
    -- case branch binds
    ]

functionArgs :: Assertion
functionArgs =
  test1 1 True @?= ["1", "True"]

test1 :: Int -> Bool -> [String]
test1 i b = traceVars

whereBinds :: Assertion
whereBinds =
  test2 @?= ["True", "1"]

test2 :: [String]
test2 = traceVars
  where a = 1 :: Int
        b = True

argsAndWhere :: Assertion
argsAndWhere =
  test3 1 @?= ["3", "2", "1"]

test3 :: Int -> [String]
test3 a = traceVars
  where b = 2 :: Int
        c = 3 :: Int

letBindings :: Assertion
letBindings =
  test4 @?= ["1", "True"]

test4 :: [String]
test4 =
  let a = True
      b = 1 :: Int
   in traceVars
