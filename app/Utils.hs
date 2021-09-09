{-# LANGUAGE TemplateHaskell #-}

module Utils where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString.Builder (toLazyByteString, word16BE)
import Data.ByteString.Lazy (unpack)
import Data.List (uncons)
import Data.Word (Word16, Word64, Word8)
import Lens.Micro (over, set, (&))
import Lens.Micro.TH (makeLenses)
import SDL
import Text.Printf (printf)
import Prelude hiding (max)

type Byte = Word8

type Program = [Byte]

type Vx = Byte

type Vy = Byte

type Nibble = Byte

type Addr = Word16

type Keymap = [Scancode]

type PixelRow = Word64

data Err = StackOverflow | StackUnderflow deriving (Show)

data Stack a = Stack
  { max :: Int,
    _items :: [a]
  }
  deriving (Show)

makeLenses ''Stack

newStack :: Int -> Stack a
newStack n =
  Stack
    { max = n,
      _items = []
    }

push :: a -> Stack a -> Stack a
push a s
  | length (_items s) == max s = error $ show StackOverflow
  | otherwise = s & over items (a :)

view :: Stack a -> (a, [a])
view s = do
  case uncons $ _items s of
    Nothing -> error $ show StackUnderflow
    Just t -> t

pop :: Stack a -> (Stack a, a)
pop s = do
  let t = view s
  (s & set items (snd t), fst t)

peek :: Stack a -> a
peek s = fst $ view s

merge :: Byte -> Byte -> Addr
merge h l =
  let high = fromIntegral h :: Addr
      low = fromIntegral l :: Addr
   in shiftL high 8 .|. low

split :: Addr -> [Byte]
split = unpack . toLazyByteString . word16BE

left :: Byte -> Byte
left b = shiftR (b .&. 0xF0) 4

right :: Byte -> Byte
right = (.&. 0x0F)

show1 :: Byte -> String
show1 = printf "%X"

show2 :: Byte -> String
show2 = printf "%02X"

show3 :: Addr -> String
show3 = printf "%03X"

show4 :: Addr -> String
show4 = printf "%04X"

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
  [ ScancodeX,
    Scancode1,
    Scancode2,
    Scancode3,
    ScancodeQ,
    ScancodeW,
    ScancodeE,
    ScancodeA,
    ScancodeS,
    ScancodeD,
    ScancodeZ,
    ScancodeC,
    Scancode4,
    ScancodeR,
    ScancodeF,
    ScancodeV
  ]

font :: [Byte]
font =
  [ 0xF0, -- 0
    0x90,
    0x90,
    0x90,
    0xF0,
    0x20, -- 1
    0x60,
    0x20,
    0x20,
    0x70,
    0xF0, -- 2
    0x10,
    0xF0,
    0x80,
    0xF0,
    0xF0, -- 3
    0x10,
    0xF0,
    0x10,
    0xF0,
    0x90, -- 4
    0x90,
    0xF0,
    0x10,
    0x10,
    0xF0, -- 5
    0x80,
    0xF0,
    0x10,
    0xF0,
    0xF0, -- 6
    0x80,
    0xF0,
    0x90,
    0xF0,
    0xF0, -- 7
    0x10,
    0x20,
    0x40,
    0x40,
    0xF0, -- 8
    0x90,
    0xF0,
    0x90,
    0xF0,
    0xF0, -- 9
    0x90,
    0xF0,
    0x10,
    0xF0,
    0xF0, -- A
    0x90,
    0xF0,
    0x90,
    0x90,
    0xE0, -- B
    0x90,
    0xE0,
    0x90,
    0xE0,
    0xF0, -- C
    0x80,
    0x80,
    0x80,
    0xF0,
    0xE0, -- D
    0x90,
    0x90,
    0x90,
    0xE0,
    0xF0, -- E
    0x80,
    0xF0,
    0x80,
    0xF0,
    0xF0, -- F
    0x80,
    0xF0,
    0x80,
    0x80
  ]