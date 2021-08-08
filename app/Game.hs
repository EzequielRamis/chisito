{-# LANGUAGE TemplateHaskell #-}

module Game where

import qualified Data.Vector.Unboxed as V
import Decode
import Helpers
import Lens.Micro.Platform

type Registers = V.Vector Byte

type Memory = V.Vector Byte

type Display = V.Vector Bool

type Keyboard = V.Vector Bool

data Game = Game
  { _pc :: Addr,
    _sp :: Byte,
    _i :: Addr,
    _dt :: Byte,
    _st :: Byte,
    _memory :: Memory,
    _stack :: Stack Addr,
    _registers :: Registers,
    _display :: Display,
    _keyboard :: Keyboard
  }

clear :: Display
clear = V.replicate (32 * 64) False

newGame :: Game
newGame =
  Game
    { _pc = 0x200,
      _sp = 0x0,
      _i = 0x0,
      _dt = 0x0,
      _st = 0x0,
      _memory = V.replicate 4096 0x0,
      _stack = new 16,
      _registers = V.replicate 16 0x0,
      _display = clear,
      _keyboard = V.replicate 16 False
    }

makeLenses ''Game

exec :: Opcode -> Game -> Either Err Game
{-

   --------------------
  | Clear the display |
  --------------------
-}
exec Cls g = Right $ g & set display clear
{-

   ---------------------------
  | Return from a subroutine |
  ---------------------------
-}
exec Ret g = do
  a <- peek $ _stack g
  return $
    g & set pc a
      & over sp (\p -> p - 1)
{-

   -----------------------
  | Jump to location nnn |
  -----------------------
-}
exec (Jp addr) g = Right $ g & set pc addr
{-

   -------------------------
  | Call subroutine at nnn |
  -------------------------
-}
exec (Call addr) g = do
  s <- _pc g `push` _stack g
  return $
    g & over sp (+ 1)
      & set stack s
      & set pc addr
exec _ _ = undefined