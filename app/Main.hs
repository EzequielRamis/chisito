module Main where

import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString as B
import GHC.Conc
import Game
import Graphics.Vty
import Utils

second :: Int
second = 1000000

sixtyHz :: Int
sixtyHz = div second 60

ticksPerSec :: Int -> Int
ticksPerSec = div second

main :: IO ()
main = do
  cfg <- standardIOConfig
  game <- newGame [] cfg
  _ <-
    forkIO $ do
      forever $ do
        threadDelay second
        d <- readMVar $ _dt game
        let line = string defAttr $ show d
            pic = picForImage line
        update (_tui game) pic
        swapMVar (_dt game) 100
  _ <-
    forever $ do
      threadDelay sixtyHz
      let decrement t = do
            x <- readMVar t
            if x == 0 then swapMVar t 0 else swapMVar t $ x - 1
      _ <- decrement $ _dt game
      decrement $ _st game
  return ()

program :: B.ByteString -> Program
program = swapEnd . B.unpack