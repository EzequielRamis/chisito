{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types where

import Control.Concurrent.MVar (MVar)
import Data.Aeson.Types
  ( FromJSON (..),
    Value (Array, String),
    typeMismatch,
  )
import Data.Text (unpack)
import Data.Vector (toList)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word16, Word64, Word8)
import Keyboard (parseKeycode)
import Language.Haskell.TH.Syntax (Exp (RecConE), Lift (..), mkName, unsafeTExpCoerce)
import Refined (Positive, Refined, SizeEqualTo)
import SDL

type Byte = Word8

type Program = [Byte]

type Vx = Byte

type Vy = Byte

type Nibble = Byte

type Addr = Word16

type PixelRow = Word64

type Color = V4 Byte

type Registers = V.Vector Byte

type Memory = V.Vector Byte

type Display = MV.IOVector PixelRow

type Timer = MVar Byte

type Keypad = MV.IOVector Bool

type Tick = Refined Positive Double

type Keymap = Refined (SizeEqualTo 16) [Keycode]

instance FromJSON Keycode where
  parseJSON (String t) = case parseKeycode t of
    KeycodeUnknown -> fail $ "parsing Keycode failed, the following value is unknown: " <> unpack t
    _ -> pure $ parseKeycode t
  parseJSON err = typeMismatch "String" err
  parseJSONList (Array t) = mapM parseJSON $ toList t
  parseJSONList t = typeMismatch "Array" t

instance Lift Keycode where
  liftTyped = unsafeTExpCoerce . lift
  lift key = do
    code <- qCode
    return $ RecConE keyName [(codeName, code)]
    where
      qCode = lift $ unwrapKeycode key
      keyName = mkName "Keycode"
      codeName = mkName "unwrapKeycode"