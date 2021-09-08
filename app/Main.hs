{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar (readMVar, swapMVar)
import Control.Monad (forever, unless, void)
import qualified Data.ByteString as B
import Decode (decode)
import GHC.Conc (forkIO, threadDelay)
import Game (Game (..), Timer, exec, fetch, newGame, suc)
import SDL hiding (Timer)
import UI (render)
import Utils (Program)

main :: IO ()
main = do
  bytecode <- B.readFile "games/MAZE"
  load $ program bytecode

load :: Program -> IO ()
load p = do
  game <- newGame p
  --
  initialize [InitVideo, InitEvents]
  window <- createWindow "Chisito" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  --
  (void . forkIO . forever) $
    do
      threadDelay $ hz 60
      decrement $ _dt game
      decrement $ _st game
  --
  play game renderer
  destroyWindow window

play :: Game -> Renderer -> IO ()
play g renderer = do
  threadDelay $ hz 700
  let bytes = fetch g
  let g' = suc g
  let mop = decode bytes
  case mop of
    Just op -> do
      next <- exec op g'
      render op g' renderer
      play next renderer
    Nothing -> error $ show bytes

program :: B.ByteString -> Program
program = B.unpack

second :: Int
second = 1000000

hz :: Int -> Int
hz = div second

decrement :: Timer -> IO ()
decrement t = do
  x <- readMVar t
  if x == 0
    then return ()
    else void $ swapMVar t $ x - 1