{-# LANGUAGE TemplateHaskell #-}

module Utils where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString.Builder (toLazyByteString, word16BE)
import Data.ByteString.Lazy (unpack)
import Data.List (elemIndex, uncons)
import Data.Word (Word16, Word64, Word8)
import Graphics.Vty.Input.Events
import Lens.Micro.Platform (makeLenses, over, set, (&))
import Text.Printf (printf)
import Prelude hiding (max)

type Byte = Word8

type Program = [Byte]

type Vx = Byte

type Vy = Byte

type Nibble = Byte

type Addr = Word16

type Keymap = [Key]

type PixelRow = Word64

data Err = StackOverflow | StackUnderflow

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

push :: a -> Stack a -> Either Err (Stack a)
push a s
  | length (_items s) == max s = Left StackOverflow
  | otherwise = return $ s & over items (a :)

view :: Stack a -> Either Err (a, [a])
view s = do
  case uncons $ _items s of
    Nothing -> Left StackUnderflow
    Just t -> return t

pop :: Stack a -> Either Err (Stack a, a)
pop s = do
  t <- view s
  return (s & set items (snd t), fst t)

peek :: Stack a -> Either Err a
peek s = do
  t <- view s
  return $ fst t

swapEnd :: [Byte] -> [Byte]
swapEnd [] = []
swapEnd [x] = [x]
swapEnd (x : y : xs) = y : x : swapEnd xs

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

keyVal :: Key -> Keymap -> Maybe Byte
keyVal k kp = Just fromIntegral <*> elemIndex k kp

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
  [ KChar 'x',
    KChar '1',
    KChar '2',
    KChar '3',
    KChar 'q',
    KChar 'w',
    KChar 'e',
    KChar 'a',
    KChar 's',
    KChar 'd',
    KChar 'z',
    KChar 'c',
    KChar '4',
    KChar 'r',
    KChar 'f',
    KChar 'v'
  ]

fontStartAddr :: Addr
fontStartAddr = 0x50

fontHeight :: Addr
fontHeight = 5