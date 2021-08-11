{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.List (uncons)
import Data.Word (Word16, Word8)
import Lens.Micro.Platform (makeLenses, over, set, (&))
import Prelude hiding (max)

type Byte = Word8

type Program = [Byte]

type Vx = Byte

type Vy = Byte

type Nibble = Byte

type Addr = Word16

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