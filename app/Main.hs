{-# LANGUAGE OverloadedStrings #-}

module Main where

import Audio (beep, newDevice, pauseBeep, playBeep)
import Config
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
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Vector.Unboxed as V
import Decode (decode)
import Game (Game (..), exec, fetch, newGame, suc)
import Refined (unrefine)
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
import Turtle (encodeString, options)
import Types
import UI (render)
import Utils

main :: IO ()
main = do
  chisito <- options "A little chip-8 interpreter" cli >>= orDefault
  bytestr <- B.readFile $ encodeString $ gameFile chisito
  config <- readGameConfig $ fromJust $ configFile chisito
  load (program bytestr) (getGameConfig config)

load :: Program -> GameConfig -> IO ()
load p c = do
  game <- newGame p
  -- Init timers
  void $ forkIO $ forever $ threadDelay (hz 60) >> decrement (_dt game)
  void $ forkIO $ forever $ threadDelay (hz 60) >> decrement (_st game)
  --
  void $ forkIO $ play game c
  initWindow game c

play :: Game -> GameConfig -> IO ()
play g c = do
  threadDelay $ hz $ unrefine $ fromJust $ tick c
  let bytes = fetch g
  let g' = suc g
  let mop = decode bytes
  case mop of
    Just op -> do
      next <- exec op g'
      play next c
    Nothing -> error $ show bytes

initWindow :: Game -> GameConfig -> IO ()
initWindow g c = do
  initialize [InitVideo, InitEvents, InitAudio]
  window <- createWindow "Chisito" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  device <- newDevice beep
  refreshIO renderer device (fromIntegral <$> windowInitialSize defaultWindow) g c
  destroyWindow window

refreshIO :: Renderer -> AudioDevice -> V2 Int32 -> Game -> GameConfig -> IO ()
refreshIO r d s g c = do
  threadDelay $ hz 60
  events <- pollEvents
  -- Screen
  let screen = getScreenSize events s
  render screen g c r
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
  unless (quitPressed events) $ refreshIO r d screen g c

keymapping :: KeyboardEventData -> Maybe Int
keymapping ke = elemIndex (keysymKeycode $ keyboardEventKeysym ke) $ unrefine defaultKeymap

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