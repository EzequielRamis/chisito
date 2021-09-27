{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Aeson.Types (FromJSON)
import Data.Word (Word8)
import GHC.Generics
import Turtle
import Types
import Prelude hiding (FilePath)

data Chisito = Chisito
  { fileGame :: FilePath,
    fileConf :: FilePath,
    debug :: Bool
  }

data GameConfig = GameConfig
  { tick :: Maybe Tick,
    background :: Maybe RGB,
    foreground :: Maybe RGB,
    keymap :: Maybe Keymap
  }
  deriving (Generic, Show)

data RGB = RGB
  { red :: Word8,
    green :: Word8,
    blue :: Word8
  }
  deriving (Generic, Show)

instance FromJSON GameConfig

instance FromJSON RGB

cli :: Parser Chisito
cli =
  Chisito
    <$> argPath "game" "Chip-8 game filepath"
    <*> optPath "config" 'c' "Configuration filepath (.yaml | .json)"
    <*> switch "debug" 'd' "Enable debugging mode"