module Audio where

import Foreign.C (CInt)
import SDL.Mixer
import SDL.Raw.Mixer hiding (Channel, Chunk, openAudio, pause, pauseMusic, pausedMusic, playingMusic, resume, resumeMusic)
import System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)

initBeep :: IO ()
initBeep = newBeep >>= playBeep >> pauseBeep

newBeep :: IO Chunk
newBeep = getXdgDirectory XdgConfig "chisito/beep.wav" >>= newSound

newSound :: FilePath -> IO Chunk
newSound file = openAudio def 256 >> load file

playBeep :: Chunk -> IO ()
playBeep = playForever

pauseBeep :: IO ()
pauseBeep = pause AllChannels

resumeBeep :: IO ()
resumeBeep = resume AllChannels

def :: Audio
def =
  Audio
    { audioFrequency = SDL.Raw.Mixer.DEFAULT_FREQUENCY,
      audioFormat = wordToFormat SDL.Raw.Mixer.DEFAULT_FORMAT,
      audioOutput = cIntToOutput SDL.Raw.Mixer.DEFAULT_CHANNELS
    }

wordToFormat :: SDL.Raw.Mixer.Format -> SDL.Mixer.Format
wordToFormat SDL.Raw.Mixer.AUDIO_U8 = FormatU8
wordToFormat SDL.Raw.Mixer.AUDIO_S8 = FormatS8
wordToFormat SDL.Raw.Mixer.AUDIO_U16LSB = FormatU16_LSB
wordToFormat SDL.Raw.Mixer.AUDIO_S16LSB = FormatS16_LSB
wordToFormat SDL.Raw.Mixer.AUDIO_U16MSB = FormatU16_MSB
wordToFormat SDL.Raw.Mixer.AUDIO_S16MSB = FormatS16_MSB
wordToFormat SDL.Raw.Mixer.AUDIO_U16SYS = FormatU16_Sys
wordToFormat SDL.Raw.Mixer.AUDIO_S16SYS = FormatS16_Sys
wordToFormat _ = error "SDL.Mixer.wordToFormat: unknown Format."

cIntToOutput :: CInt -> Output
cIntToOutput 1 = Mono
cIntToOutput 2 = Stereo
cIntToOutput _ = error "SDL.Mixer.cIntToOutput: unknown number of channels."
