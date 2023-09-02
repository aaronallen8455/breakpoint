{-# LANGUAGE DataKinds #-}
import           Control.Concurrent
import           Control.Monad
import           Debug.Breakpoint
import           Data.Fixed

main :: IO ()
main = do
  forkIO $ putStrLn "in fork"
  forever $ do
    threadDelay 1000000
    putStrLn "looping"
