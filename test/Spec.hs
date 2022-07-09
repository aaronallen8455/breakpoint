import qualified Data.Map as M
import           Test.Tasty
import           Test.Tasty.HUnit

import           Debug.BreakPoint

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup "Tests"
--     [ testCase "function args" functionArgs
--     , testCase "where binds" whereBinds
--     , testCase "args and where" argsAndWhere
--     , testCase "let bindings" letBindings
--     , testCase "nested in let" nestedInLet
    [ testCase "let scoping" letScoping]
--     , testCase "nested in where" nestedInWhere
--     , testCase "guard binding" guardBinding
--     , testCase "pattern guard" patternGuard
--     , testCase "lambda bind" lambdaBind
--     , testCase "no let escape" noLetEscape
--     , testCase "case pattern bind" casePatBind
    -- TODO
    -- monadic binds
    -- let bind in do
    -- case branch binds
    -- binds in a list comprehension
    -- arrow notation bindings

-- functionArgs :: Assertion
-- functionArgs = test1 1 True @?= M.fromList [("b", "True"), ("i", "1")]
-- 
-- test1 :: Int -> Bool -> M.Map String String
-- test1 i b = traceVars
-- 
-- whereBinds :: Assertion
-- whereBinds = test2 @?= M.fromList [("a", "1"), ("b", "True")]
-- 
-- test2 :: M.Map String String
-- test2 = traceVars
--   where a = 1 :: Int
--         b = True
-- 
-- argsAndWhere :: Assertion
-- argsAndWhere = test3 1 @?= M.fromList [("a", "1"), ("b", "2"), ("c", "3")]
-- 
-- test3 :: Int -> M.Map String String
-- test3 a = traceVars
--   where b = 2 :: Int
--         c = 3 :: Int
-- 
-- letBindings :: Assertion
-- letBindings =
--   test4 @?= M.fromList [("a", "True"), ("b", "1")]
-- 
-- test4 :: M.Map String String
-- test4 =
--   let a = True
--       b = 1 :: Int
--    in traceVars
-- 
-- nestedInLet :: Assertion
-- nestedInLet = test5 1 @?= M.fromList [("a", "1"), ("b", "2"), ("c", "3")]
-- 
-- test5 :: Int -> M.Map String String
-- test5 a =
--   let x =
--         let b = 2
--             c = 3
--          in traceVars
--    in x
-- 
-- noLetEscape :: Assertion
-- noLetEscape = test6 @?= M.fromList [("a", "()")]
-- 
-- test6 :: M.Map String String
-- test6 =
--   let a =
--         let b = True
--             c = False
--          in ()
--    in traceVars
-- 
-- nestedInWhere :: Assertion
-- nestedInWhere = test7 @?= M.fromList [("a", "()")]
-- 
-- test7 :: M.Map String String
-- test7 = traceVars where
--   a = () where
--           b = True
--           c = False
-- 
-- guardBinding :: Assertion
-- guardBinding = test8 @?= M.fromList [("a", "()"), ("b", "()")]
-- 
-- test8 :: M.Map String String
-- test8 | let a = (), let b = a = traceVars
-- 
-- patternGuard :: Assertion
-- patternGuard = test9 @?= M.fromList [("a", "()")]
-- 
-- test9 :: M.Map String String
-- test9 | Just a <- Just () = traceVars
-- 
-- lambdaBind :: Assertion
-- lambdaBind = test10 () @?= M.fromList [("a", "()")]
-- 
-- test10 :: () -> M.Map String String
-- test10 = \a -> traceVars
-- 
letScoping :: Assertion
letScoping = test11 @?= M.fromList [("b", "True"), ("c", "False")]

test11 :: M.Map String String
test11 =
  let a = traceVars
      b = True
      c = False
   in a
-- 
-- casePatBind :: Assertion
-- casePatBind = test12 @?= M.fromList [("a", "()")]
-- 
-- test12 :: M.Map String String
-- test12 = case Just () of
--            Just a -> traceVars

-- test13 :: M.Map String String
-- test13 =
--   let b = True
--       a = traceVars
--    in mempty
