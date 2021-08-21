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
  bytecode <- B.readFile "test/opcode"
  cfg <- standardIOConfig
  game <- newGame cfg $ program bytecode
  _ <-
    forkIO $ do
      forever $ do
        threadDelay $ hz 60
        _ <- decrement $ _dt game
        decrement $ _st game
  play game

program :: B.ByteString -> Program
program = B.unpack

play :: Game -> IO ()
play game = do
  threadDelay $ hz 1
  let bytes = fetch game
  let game' = next game
  let mop = decode bytes
  print $ show game' ++ show bytes ++ show mop
  case mop of
    Just op -> do
      res <- exec op game'
      case res of
        Left e -> error $ show e
        Right suc -> play suc
    Nothing -> error "asdsad"

second :: Int
second = 1000000

hz :: Int -> Int
hz = div second

decrement :: Timer -> IO Byte
decrement t = do
  x <- readMVar t
  if x == 0 then return x else swapMVar t $ x - 1