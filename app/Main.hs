{-# LANGUAGE OverloadedStrings #-}

module Main where

import Audio (initBeep, pauseBeep, resumeBeep)
import Config
  ( Chisito (configFile, gameFile),
    GameConfig (..),
    cli,
    defaultKeymap,
    getGameConfig,
    orDefault,
    readGameConfig,
  )
import Control.Concurrent
  ( forkIO,
    readMVar,
    swapMVar,
    threadDelay,
  )
import Control.Monad (forever, unless, void)
import qualified Data.ByteString as B
import Data.Int (Int32)
import Data.List (elemIndex, find)
import Data.Maybe (fromJust, mapMaybe)
import Data.Text (append, pack)
import qualified Data.Vector.Unboxed as V
import Decode (decode)
import Game (Game (..), exec, fetch, newGame, suc)
import Refined (unrefine)
import SDL
  ( Event (eventPayload),
    EventPayload (KeyboardEvent, QuitEvent, WindowResizedEvent),
    InitFlag (InitAudio, InitEvents, InitVideo),
    InputMotion (Pressed),
    KeyboardEventData (keyboardEventKeyMotion, keyboardEventKeysym),
    Keysym (keysymKeycode),
    Renderer,
    V2,
    WindowConfig (..),
    WindowMode (..),
    WindowResizedEventData (windowResizedEventSize),
    createRenderer,
    createWindow,
    defaultRenderer,
    defaultWindow,
    destroyWindow,
    initialize,
    pollEvents,
  )
import qualified SDL.Mixer as Mix
import Turtle (FilePath, basename, encodeString, options)
import Types (Program, Tick, Timer)
import UI (render)
import Utils (hz)
import Prelude hiding (FilePath)

main :: IO ()
main = do
  chisito <- orDefault =<< options "A little chip-8 interpreter" cli
  bytestr <- B.readFile $ encodeString $ gameFile chisito
  config <- readGameConfig $ fromJust $ configFile chisito
  load (program bytestr) (getGameConfig config) (basename $ gameFile chisito)

load :: Program -> GameConfig -> FilePath -> IO ()
load p c t = do
  game <- newGame p
  let initTimer d d' = void $ forkIO $ forever $ delay d >> decrement d'
  initTimer (delayTimer c) (_dt game)
  initTimer (soundTimer c) (_st game)
  void $ forkIO $ play game c
  initWindow game c t

play :: Game -> GameConfig -> IO ()
play g c = do
  delay $ tick c
  let bytes = fetch g
  let g' = suc g
  let mop = decode bytes
  case mop of
    Just op -> do
      next <- exec op g'
      play next c
    Nothing -> error $ show bytes

initWindow :: Game -> GameConfig -> FilePath -> IO ()
initWindow g c t = do
  initialize [InitVideo, InitEvents, InitAudio]
  Mix.initialize [Mix.InitMP3]
  window <- createWindow (append "Chisito - " $ pack $ encodeString t) (getWindowSize $ windowSize c)
  renderer <- createRenderer window (-1) defaultRenderer
  initBeep
  refreshIO renderer (fromIntegral <$> windowInitialSize defaultWindow) g c
  destroyWindow window

refreshIO :: Renderer -> V2 Int32 -> Game -> GameConfig -> IO ()
refreshIO r s g c = do
  delay $ fps c
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
  if st == 0
    then pauseBeep
    else resumeBeep
  --
  unless (quitPressed events) $ refreshIO r screen g c

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

delay :: Maybe Tick -> IO ()
delay t = threadDelay $ hz $ unrefine $ fromJust t

getWindowSize :: Maybe Int -> WindowConfig
getWindowSize (Just 0) = defaultChisitoWindow
getWindowSize (Just 1) = defaultChisitoWindow {windowMode = Maximized}
getWindowSize (Just 2) = defaultChisitoWindow {windowMode = FullscreenDesktop}
getWindowSize (Just 3) = defaultChisitoWindow {windowMode = Fullscreen}
getWindowSize _ = defaultChisitoWindow

defaultChisitoWindow :: WindowConfig
defaultChisitoWindow = defaultWindow {windowResizable = True, windowHighDPI = True}