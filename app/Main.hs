module Main where

import Control.Concurrent.MVar (readMVar, swapMVar)
import Control.Monad (forever)
import qualified Data.ByteString as B
import Data.Char (toLower)
import qualified Data.Vector.Unboxed.Mutable as MV
import Decode (decode)
import GHC.Conc (forkIO, threadDelay)
import Game (Game (..), Timer, exec, fetch, newGame, suc)
import Graphics.Vty
  ( Event (EvKey),
    Key (KChar),
    Vty (nextEventNonblocking),
    mkVty,
    standardIOConfig,
  )
import UI (refresh)
import Utils

main :: IO ()
main = do
  bytecode <- B.readFile "games/BRIX"
  cfg <- standardIOConfig
  vty <- mkVty cfg
  game <- newGame $ program bytecode
  _ <-
    forkIO $ do
      forever $ do
        threadDelay $ hz 60
        _ <- decrement $ _dt game
        decrement $ _st game
  _ <-
    forkIO $ do
      forever $ do
        threadDelay $ hz 60
        me <- nextEventNonblocking vty
        case me of
          Just e -> do
            case e of
              EvKey (KChar k) _ -> do
                case keyVal (KChar $ toLower k) defaultKeymap of
                  Just b -> do
                    alreadyPressed <- MV.read (_keypad game) (fromIntegral b)
                    if alreadyPressed
                      then return ()
                      else do
                        MV.write (_keypad game) (fromIntegral b) True
                        _ <- forkIO $ do
                          threadDelay $ hz 5
                          MV.write (_keypad game) (fromIntegral b) False
                        return ()
                  Nothing -> return ()
              _ -> return ()
          Nothing -> return ()
  play game vty

program :: B.ByteString -> Program
program = B.unpack

play :: Game -> Vty -> IO ()
play game vty = do
  threadDelay $ hz 700
  let bytes = fetch game
  let game' = suc game
  let mop = decode bytes
  case mop of
    Just op -> do
      next <- exec op game'
      _ <- refresh vty next op
      play next vty
    Nothing -> error $ show bytes

second :: Int
second = 1000000

hz :: Int -> Int
hz = div second

decrement :: Timer -> IO Byte
decrement t = do
  x <- readMVar t
  if x == 0 then return x else swapMVar t $ x - 1