{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar (readMVar, swapMVar)
import Control.Monad (forever, unless, void)
import qualified Data.ByteString as B
import Data.List (elemIndex)
import Data.Maybe (mapMaybe)
import qualified Data.Vector.Unboxed as V
import Decode (decode)
import GHC.Conc (forkIO, threadDelay)
import Game (Game (..), Timer, exec, fetch, newGame, suc)
import SDL hiding (Timer)
import UI (render)
import Utils

main :: IO ()
main = do
  bytecode <- B.readFile "games/BRIX"
  load $ program bytecode

load :: Program -> IO ()
load p = do
  game <- newGame p
  -- Init timers
  void $
    forkIO $
      forever $ do
        threadDelay $ hz 60
        decrement $ _dt game
        decrement $ _st game
  --
  void $ forkIO $ play game
  initWindow game

initWindow :: Game -> IO ()
initWindow g = do
  initialize [InitVideo, InitEvents]
  window <- createWindow "Chisito" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  refreshIO renderer g
  destroyWindow window

refreshIO :: Renderer -> Game -> IO ()
refreshIO r g = do
  threadDelay $ hz 60
  render g r
  --
  events <- pollEvents
  let keyEvents = mapMaybe eventToKey events
  oldKeys <- V.freeze $ _keypad g
  let newKeys = oldKeys V.// keyEvents
  V.copy (_keypad g) newKeys
  --
  unless (quitPressed events) $ refreshIO r g

keymapping :: KeyboardEventData -> Maybe Int
keymapping ke = elemIndex (keysymScancode $ keyboardEventKeysym ke) defaultKeymap

eventToKey :: Event -> Maybe (Int, Bool)
eventToKey e = case eventPayload e of
  KeyboardEvent ke -> do
    i <- keymapping ke
    Just (i, keyboardEventKeyMotion ke == Pressed)
  _ -> Nothing

quitPressed :: [Event] -> Bool
quitPressed = any eventIsQPress
  where
    eventIsQPress event = case eventPayload event of
      QuitEvent -> True
      _ -> False

play :: Game -> IO ()
play g = do
  threadDelay $ hz 700
  let bytes = fetch g
  let g' = suc g
  let mop = decode bytes
  case mop of
    Just op -> do
      next <- exec op g'
      play next
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