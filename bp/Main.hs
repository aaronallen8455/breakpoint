{-# LANGUAGE DataKinds #-}
import           Debug.Breakpoint
import           Data.Fixed

main :: IO ()
main = do
  i <- getLine
  let x = 9 :: Fixed 100
  putStrLn i
  breakpointIO
  putStrLn $ succ <$> i
