module Main where

import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString as B
import Decode
import GHC.Conc
import Game
import Graphics.Vty
import Utils

main :: IO ()
main = do
  bytecode <- B.readFile ""
  cfg <- standardIOConfig
  game <- newGame cfg $ program bytecode
  _ <-
    forkIO $ do
      forever $ do
        threadDelay $ hz 60
        _ <- decrement $ _dt game
        decrement $ _st game
  _ <-
    forever $ do
      threadDelay $ hz 700
      let bytes = fetch game
      case decode bytes of
        Just op -> do
          _ <- exec op game
          next game
        Nothing -> next game
  return ()

program :: B.ByteString -> Program
program = swapEnd . B.unpack

second :: Int
second = 1000000

hz :: Int -> Int
hz = div second

decrement :: Timer -> IO Byte
decrement t = do
  x <- readMVar t
  if x == 0 then return x else swapMVar t $ x - 1