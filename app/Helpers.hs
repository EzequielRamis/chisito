{-# LANGUAGE TemplateHaskell #-}

module Helpers where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString.Builder (toLazyByteString, word16BE)
import Data.ByteString.Lazy (unpack)
import Data.List (uncons)
import Data.Word (Word16, Word8)
import Lens.Micro.Platform (makeLenses, over, set, (&))
import Text.Printf (printf)
import Prelude hiding (max)

type Program = [Word8]

type Vx = Word8

type Vy = Word8

type Addr = Word16

type Nibble = Word8

type Byte = Word8

data Err = StackOverflow | StackUnderflow

data Stack a = Stack
  { max :: Int,
    _items :: [a]
  }
  deriving (Show)

makeLenses ''Stack

new :: Int -> Stack a
new n =
  Stack
    { max = n,
      _items = []
    }

push :: a -> Stack a -> Either Err (Stack a)
push a s
  | length (_items s) == max s = Left StackOverflow
  | otherwise =
    return $
      s & over items (a :)

view :: Stack a -> Either Err (a, [a])
view s = do
  case uncons $ _items s of
    Nothing -> Left StackUnderflow
    Just t -> return t

pop :: Stack a -> Either Err (Stack a, a)
pop s = do
  t <- view s
  return
    ( s & set items (snd t),
      fst t
    )

peek :: Stack a -> Either Err a
peek s = do
  t <- view s
  return $ fst t

swapEnd :: [Word8] -> [Word8]
swapEnd [] = []
swapEnd [x] = [x]
swapEnd (x : y : xs) = y : x : swapEnd xs

merge :: Word8 -> Word8 -> Word16
merge h l =
  let high = fromIntegral h :: Word16
      low = fromIntegral l :: Word16
   in shiftL high 8 .|. low

split :: Word16 -> [Word8]
split = unpack . toLazyByteString . word16BE

left :: Word8 -> Word8
left b = shiftR (b .&. 0xF0) 4

right :: Word8 -> Word8
right = (.&. 0x0F)

show1 :: Word8 -> String
show1 = printf "%X"

show2 :: Word8 -> String
show2 = printf "%02X"

show3 :: Word16 -> String
show3 = printf "%03X"

show4 :: Word16 -> String
show4 = printf "%04X"
