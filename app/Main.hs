{-# LANGUAGE OverloadedStrings #-}

module Main where

import Audio (beep, newDevice, pauseBeep, playBeep)
import Control.Concurrent
  ( forkIO,
    readMVar,
    swapMVar,
    threadDelay,
  )
import Control.Monad (forever, unless, void, when)
import qualified Data.ByteString as B
import Data.Int (Int32)
import Data.List (elemIndex, find)
import Data.Maybe (mapMaybe)
import qualified Data.Vector.Unboxed as V
import Decode (decode)
import Game (Game (..), Timer, exec, fetch, newGame, suc)
import SDL
  ( AudioDevice,
    AudioDeviceStatus (Paused, Playing),
    Event (eventPayload),
    EventPayload (KeyboardEvent, QuitEvent, WindowResizedEvent),
    InitFlag (InitAudio, InitEvents, InitVideo),
    InputMotion (Pressed),
    KeyboardEventData (keyboardEventKeyMotion, keyboardEventKeysym),
    Keysym (keysymKeycode),
    Renderer,
    V2,
    WindowConfig (windowInitialSize),
    WindowResizedEventData (windowResizedEventSize),
    audioDeviceStatus,
    createRenderer,
    createWindow,
    defaultRenderer,
    defaultWindow,
    destroyWindow,
    initialize,
    pollEvents,
  )
import UI (render)
import Utils (Program, defaultKeymap, hz)

main :: IO ()
main = do
  bytecode <- B.readFile "games/PONG2"
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

initWindow :: Game -> IO ()
initWindow g = do
  initialize [InitVideo, InitEvents, InitAudio]
  window <- createWindow "Chisito" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  device <- newDevice beep
  refreshIO renderer device (fromIntegral <$> windowInitialSize defaultWindow) g
  destroyWindow window

refreshIO :: Renderer -> AudioDevice -> V2 Int32 -> Game -> IO ()
refreshIO r d s g = do
  threadDelay $ hz 60
  events <- pollEvents
  -- Screen
  let screen = getScreenSize events s
  render screen g r
  -- Keypad
  let keyEvents = mapMaybe eventToKey events
  oldKeys <- V.freeze $ _keypad g
  let newKeys = oldKeys V.// keyEvents
  V.copy (_keypad g) newKeys
  -- Sound Timer
  st <- readMVar (_st g)
  status <- audioDeviceStatus d
  if st == 0
    then when (status == Playing) $ pauseBeep d
    else when (status == Paused) $ playBeep d
  --
  unless (quitPressed events) $ refreshIO r d screen g

keymapping :: KeyboardEventData -> Maybe Int
keymapping ke = elemIndex (keysymKeycode $ keyboardEventKeysym ke) defaultKeymap

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

getScreenSize :: [Event] -> V2 Int32 -> V2 Int32
getScreenSize es def = case eventPayload <$> screen of
  Just (WindowResizedEvent w) -> windowResizedEventSize w
  _ -> def
  where
    screen =
      find
        ( \e -> case eventPayload e of
            WindowResizedEvent _ -> True
            _ -> False
        )
        es

program :: B.ByteString -> Program
program = B.unpack

decrement :: Timer -> IO ()
decrement t = do
  x <- readMVar t
  if x == 0
    then return ()
    else void $ swapMVar t $ x - 1