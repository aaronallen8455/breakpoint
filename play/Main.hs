{-# LANGUAGE DataKinds #-}
import           Debug.Breakpoint
import           Data.Fixed

main :: IO ()
main = do
  let x = True
  let f = 9 :: Fixed 1009
  let g = [Just Foo]
  breakpointM
  pure ()

data Foo = Foo
