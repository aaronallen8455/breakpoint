{-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}
import Control.Monad
import Control.Concurrent
import Debug.Breakpoint

main :: IO ()
main = do
  let x = True

  forkIO $ do
    let x = [MkRecord "foo" 1]
    threadDelay 500000
    breakpointIO
    putStrLn "forked"

  forever $ do
    putStrLn "looping"
    threadDelay 2000000

data Record = MkRecord
  { field1 :: String
  , field2 :: Int
  }
