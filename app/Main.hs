{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar (readMVar, swapMVar)
import Control.Monad (unless)
import qualified Data.ByteString as B
import Decode (decode)
import GHC.Conc (forkIO, threadDelay)
import Game (Game (..), Timer, exec, fetch, newGame, suc)
import SDL hiding (Timer)
import Utils (Byte, Program)

main :: IO ()
main = do
  initialize [InitVideo, InitEvents]
  window <- createWindow "Chisito" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer
  destroyWindow window

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          QuitEvent -> True
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 0 255 255 255
  clear renderer
  present renderer
  unless qPressed (appLoop renderer)

program :: B.ByteString -> Program
program = B.unpack

play :: Game -> IO ()
play game = do
  threadDelay $ hz 700
  let bytes = fetch game
  let game' = suc game
  let mop = decode bytes
  case mop of
    Just op -> do
      next <- exec op game'
      play next
    Nothing -> error $ show bytes

second :: Int
second = 1000000

hz :: Int -> Int
hz = div second

decrement :: Timer -> IO Byte
decrement t = do
  x <- readMVar t
  if x == 0 then return x else swapMVar t $ x - 1