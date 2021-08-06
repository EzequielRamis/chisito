module Opcodes where

import Data.Bits (Bits (shiftL, shiftR), (.&.), (.|.))
import Data.Word (Word16, Word8)

type Program = [Word8]

type Vx = Word8

type Vy = Word8

type Addr = Word16

type Nibble = Word8

type Byte = Word8

data Opcode
  = Ret
  | Cls
  | Jp Addr
  | Call Addr
  | SeB Vx Byte
  | SneB Vx Byte
  | Se Vx Vy
  | LdB Vx Byte
  | AddB Vx Byte
  | Ld Vx Vy
  | Or Vx Vy
  | And Vx Vy
  | Xor Vx Vy
  | Add Vx Vy
  | Sub Vx Vy
  | Shr Vx Vy
  | Subn Vx Vy
  | Shl Vx Vy
  | Sne Vx Vy
  | LdI Addr
  | JpV Addr
  | Rnd Vx Byte
  | Drw Vx Vy Nibble
  | Skp Vx
  | Sknp Vx
  | LdVDT Vx
  | LdK Vx
  | LdDTV Vx
  | LdST Vx
  | AddI Vx
  | LdFV Vx
  | LdBV Vx
  | LdIV Vx
  | LdVI Vx
  deriving (Show)

op :: (Word8, Word8) -> Opcode
op (h, l) = op' (left h, right h, l)

op' :: (Word8, Word8, Word8) -> Opcode
op' (0x0, h, l) = op0 (h, l)
op' (0x1, h, l) = Jp $ merge h l
op' (0x2, h, l) = Call $ merge h l
op' (0x3, h, l) = SeB h l
op' (0x4, h, l) = SneB h l
op' (0x5, h, l) = op5 (h, l)
op' (0x6, h, l) = LdB h l
op' (0x7, h, l) = AddB h l
op' (0x8, h, l) = op8 (h, left l, right l)
op' (0x9, h, l) = op9 (h, l)
op' (0xA, h, l) = LdI $ merge h l
op' (0xB, h, l) = JpV $ merge h l
op' (0xC, h, l) = Rnd h l
op' (0xD, h, l) = Drw h (left l) (right l)
op' (0xE, h, l) = opE (h, l)
op' (0xF, h, l) = opF (h, l)
op' (_, _, _) = error "Foo"

op0 :: (Word8, Word8) -> Opcode
op0 (0x0, 0xE0) = Cls
op0 (0x0, 0xEE) = Ret
op0 (_, _) = error "Foo"

op5 :: (Word8, Word8) -> Opcode
op5 (h, l)
  | right l == 0x0 = Se h $ left l
  | otherwise = error "Foo"

op8 :: (Word8, Word8, Word8) -> Opcode
op8 (x, y, 0x1) = Or x y
op8 (x, y, 0x2) = And x y
op8 (x, y, 0x3) = Xor x y
op8 (x, y, 0x4) = Add x y
op8 (x, y, 0x5) = Sub x y
op8 (x, y, 0x6) = Shr x y
op8 (x, y, 0x7) = Subn x y
op8 (x, y, 0xE) = Shl x y
op8 (_, _, _) = error "Foo"

op9 :: (Word8, Word8) -> Opcode
op9 (h, l)
  | right l == 0x0 = Sne h $ left l
  | otherwise = error "Foo"

opE :: (Word8, Word8) -> Opcode
opE (x, 0x9E) = Skp x
opE (x, 0xA1) = Sknp x
opE (_, _) = error "Foo"

opF :: (Word8, Word8) -> Opcode
opF (x, 0x07) = LdVDT x
opF (x, 0x0A) = LdK x
opF (x, 0x15) = LdDTV x
opF (x, 0x18) = LdST x
opF (x, 0x1E) = AddI x
opF (x, 0x29) = LdFV x
opF (x, 0x33) = LdBV x
opF (x, 0x55) = LdIV x
opF (x, 0x65) = LdVI x
opF (_, _) = error "Foo"

swapEnd :: [Word8] -> [Word8]
swapEnd [] = []
swapEnd [x] = [x]
swapEnd (x : y : xs) = y : x : swapEnd xs

merge :: Word8 -> Word8 -> Word16
merge h l =
  let high = fromIntegral h :: Word16
      low = fromIntegral l :: Word16
   in shiftL high 8 .|. low

left :: Word8 -> Word8
left b = shiftR (b .&. 0xF0) 4

right :: Word8 -> Word8
right = (.&. 0x0F)