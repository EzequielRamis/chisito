{-# LANGUAGE GADTs #-}

module Audio where

import Control.Monad (zipWithM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int16, Int32)
import qualified Data.Vector.Storable.Mutable as MV
import SDL
  ( AudioDevice,
    AudioDeviceUsage (ForPlayback),
    AudioFormat (Signed16BitLEAudio, Signed16BitNativeAudio),
    Changeable (Mandate),
    Channels (Mono),
    OpenDeviceSpec
      ( OpenDeviceSpec,
        openDeviceCallback,
        openDeviceChannels,
        openDeviceFormat,
        openDeviceFreq,
        openDeviceName,
        openDeviceSamples,
        openDeviceUsage
      ),
    PlaybackState (Pause, Play),
    openAudioDevice,
    setAudioDevicePlaybackState,
  )

newDevice :: [Int16] -> IO AudioDevice
newDevice b = do
  s <- newIORef b
  (device, _) <-
    openAudioDevice
      OpenDeviceSpec
        { SDL.openDeviceFreq =
            Mandate 48000,
          SDL.openDeviceFormat =
            Mandate Signed16BitNativeAudio,
          SDL.openDeviceChannels =
            Mandate Mono,
          SDL.openDeviceSamples = 4096,
          SDL.openDeviceCallback = audioCB s,
          SDL.openDeviceUsage = ForPlayback,
          SDL.openDeviceName = Nothing
        }
  return device

playBeep :: AudioDevice -> IO ()
playBeep device = setAudioDevicePlaybackState device Play

pauseBeep :: AudioDevice -> IO ()
pauseBeep device = setAudioDevicePlaybackState device Pause

beep :: [Int16]
beep =
  map
    ( \n ->
        let t = fromIntegral n / 48000 :: Double
            freq = 250
         in round (fromIntegral (div maxBound 2 :: Int16) * sin (2 * pi * freq * t))
    )
    [0 :: Int32 ..]

audioCB :: IORef [Int16] -> AudioFormat sampleType -> MV.IOVector sampleType -> IO ()
audioCB samples format buffer =
  case format of
    Signed16BitLEAudio ->
      do
        samples' <- readIORef samples
        let n = MV.length buffer
        zipWithM_
          (MV.write buffer)
          [0 ..]
          (take n samples')
        writeIORef
          samples
          (drop n samples')
    _ -> error "Unsupported audio format"