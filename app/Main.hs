module Main where

import Control.Concurrent.MVar (readMVar, swapMVar)
import Control.Monad (forever)
import qualified Data.ByteString as B
import Decode (decode)
import GHC.Conc (forkIO, threadDelay)
import Game (Game (_dt, _st), Timer, exec, fetch, newGame, next)
import Graphics.Vty (standardIOConfig)
import Utils (Byte, Program)

main :: IO ()
main = do
  bytecode <- B.readFile "games/PONG"
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
  threadDelay $ hz 700
  let bytes = fetch game
  let game' = next game
  let mop = decode bytes
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