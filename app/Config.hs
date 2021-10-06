{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import Control.Applicative (Alternative ((<|>)), optional)
import qualified Data.Aeson as JSON
import Data.Aeson.Types (FromJSON)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString as B
import Data.Text (append, pack, replace, unpack)
import Data.Word (Word8)
import qualified Data.Yaml as YAML
import GHC.Generics (Generic)
import Refined (refineTH)
import SDL.Input.Keyboard
import System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)
import Turtle (FilePath, Parser, argPath, decodeString, encodeString, extension, optPath, switch)
import Types (Keymap, Tick)
import Prelude hiding (FilePath)

data Chisito = Chisito
  { gameFile :: FilePath,
    configFile :: Maybe FilePath,
    debug :: Bool
  }
  deriving (Show)

data GameConfig = GameConfig
  { tick :: Maybe Tick,
    fps :: Maybe Tick,
    delayTimer :: Maybe Tick,
    soundTimer :: Maybe Tick,
    background :: Maybe RGB,
    foreground :: Maybe RGB,
    windowSize :: Maybe Int,
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

defaultConfigFile :: IO String
defaultConfigFile = getXdgDirectory XdgConfig "chisito/config.yaml"

defaultGameConfig :: GameConfig
defaultGameConfig =
  GameConfig
    { tick = Just ($$(refineTH 700) :: Tick),
      fps = Just ($$(refineTH 60) :: Tick),
      delayTimer = Just ($$(refineTH 60) :: Tick),
      soundTimer = Just ($$(refineTH 60) :: Tick),
      background = Just RGB {red = 0, green = 0, blue = 0},
      foreground = Just RGB {red = 255, green = 255, blue = 255},
      windowSize = Just 0,
      keymap = Just defaultKeymap
    }

defaultKeymap :: Keymap
-- ---------------              ---------------
--  1 | 2 | 3 | 4                1 | 2 | 3 | C
-- ---------------              ---------------
--  Q | W | E | R                4 | 5 | 6 | D
-- ---------------     --->     ---------------
--  A | S | D | F                7 | 8 | 9 | E
-- ---------------              ---------------
--  Z | X | C | V                A | 0 | B | F
-- ---------------              ---------------
defaultKeymap =
  $$( refineTH
        [ KeycodeX,
          Keycode1,
          Keycode2,
          Keycode3,
          KeycodeQ,
          KeycodeW,
          KeycodeE,
          KeycodeA,
          KeycodeS,
          KeycodeD,
          KeycodeZ,
          KeycodeC,
          Keycode4,
          KeycodeR,
          KeycodeF,
          KeycodeV
        ]
    )

orDefault :: Chisito -> IO Chisito
orDefault c = do
  config <- defaultConfigFile
  return c {configFile = configFile c <|> Just (decodeString config)}

getGameConfig :: GameConfig -> GameConfig
getGameConfig gc =
  GameConfig
    { tick = tick gc <|> tick defaultGameConfig,
      fps = fps gc <|> fps defaultGameConfig,
      delayTimer = delayTimer gc <|> delayTimer defaultGameConfig,
      soundTimer = soundTimer gc <|> soundTimer defaultGameConfig,
      background = background gc <|> background defaultGameConfig,
      foreground = foreground gc <|> foreground defaultGameConfig,
      windowSize = windowSize gc <|> windowSize defaultGameConfig,
      keymap = keymap gc <|> keymap defaultGameConfig
    }

readGameConfig :: FilePath -> IO GameConfig
readGameConfig f = do
  eitherConf <- parseConfig f
  case eitherConf of
    Left str -> do
      putStrLn "An error ocurred loading a configuration file:\n"
      putStrLn $ unpack $ append "    " $ replace "\n" "\n    " $ pack str
      putStrLn "\nLoading default configuration..."
      return defaultGameConfig
    Right gc -> do
      return $ getGameConfig gc

cli :: Parser Chisito
cli =
  Chisito
    <$> argPath "game" "Chip-8 game filepath"
    <*> optional (optPath "config" 'c' "Configuration filepath (.yaml | .json)")
    <*> switch "debug" 'd' "Enable debugging mode"

parseConfig :: FilePath -> IO (Either String GameConfig)
parseConfig f =
  let ext = extension f
   in if ext == Just "yaml" || ext == Just "yml"
        then do
          y <- fmap YAML.decodeEither' $ B.readFile $ encodeString f
          return $ first YAML.prettyPrintParseException y
        else JSON.eitherDecodeFileStrict $ encodeString f