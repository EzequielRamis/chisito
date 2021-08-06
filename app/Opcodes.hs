module Opcodes where

import Data.Bits (Bits (shiftL, shiftR), (.&.), (.|.))
import Data.ByteString.Builder (toLazyByteString, word16BE)
import Data.ByteString.Lazy (unpack)
import Data.Word (Word16, Word8)
import Text.Printf (printf)

type Program = [Word8]

type Vx = Word8

type Vy = Word8

type Addr = Word16

type Nibble = Word8

type Byte = Word8

type Decoded = Word8 -> Word8 -> Maybe Opcode

type Decoded' = Word8 -> Word8 -> Word8 -> Maybe Opcode

data Opcode
  = Sys Addr -- This one will be ignored but it's there 'cause is not invalid
  | Cls
  | Ret
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

show1 :: Word8 -> String
show1 = printf "%X"

show2 :: Word8 -> String
show2 = printf "%02X"

show3 :: Word16 -> String
show3 = printf "%03X"

show4 :: Word16 -> String
show4 = printf "%04X"

instance Show Opcode where
  show (Sys addr) = "SYS " ++ show3 addr
  show Cls = "CLS"
  show Ret = "RET"
  show (Jp addr) = "JP " ++ show3 addr
  show (Call addr) = "CALL " ++ show3 addr
  show (SeB x b) = "SE V" ++ show1 x ++ ", " ++ show2 b
  show (SneB x b) = "SNE V" ++ show1 x ++ ", " ++ show2 b
  show (Se x y) = "SE V" ++ show1 x ++ ", V" ++ show1 y
  show (LdB x b) = "LD V" ++ show1 x ++ ", " ++ show2 b
  show (AddB x b) = "ADD V" ++ show1 x ++ ", " ++ show2 b
  show (Ld x y) = "LD V" ++ show1 x ++ ", V" ++ show1 y
  show (Or x y) = "OR V" ++ show1 x ++ ", V" ++ show1 y
  show (And x y) = "AND V" ++ show1 x ++ ", V" ++ show1 y
  show (Xor x y) = "XOR V" ++ show1 x ++ ", V" ++ show1 y
  show (Add x y) = "ADD V" ++ show1 x ++ ", V" ++ show1 y
  show (Sub x y) = "SUB V" ++ show1 x ++ ", V" ++ show1 y
  show (Shr x y) = "SHR V" ++ show1 x ++ ", V" ++ show1 y
  show (Subn x y) = "SUBN V" ++ show1 x ++ ", V" ++ show1 y
  show (Shl x y) = "SHL V" ++ show1 x ++ ", V" ++ show1 y
  show (Sne x y) = "SNE V" ++ show1 x ++ ", V" ++ show1 y
  show (LdI addr) = "LD I, " ++ show3 addr
  show (JpV addr) = "JP V0, " ++ show3 addr
  show (Rnd x b) = "RND V" ++ show1 x ++ ", " ++ show2 b
  show (Drw x y n) = "DRW V" ++ show1 x ++ ", V" ++ show1 y ++ ", " ++ show1 n
  show (Skp x) = "SKP V" ++ show1 x
  show (Sknp x) = "SKNP V" ++ show1 x
  show (LdVDT x) = "LD V" ++ show1 x ++ ", DT"
  show (LdK x) = "LD V" ++ show1 x ++ ", K"
  show (LdDTV x) = "LD DT, V" ++ show1 x
  show (LdST x) = "LD ST, V" ++ show1 x
  show (AddI x) = "ADD I, V" ++ show1 x
  show (LdFV x) = "LD F, V" ++ show1 x
  show (LdBV x) = "LD B, V" ++ show1 x
  show (LdIV x) = "LD [I], V" ++ show1 x
  show (LdVI x) = "LD V" ++ show1 x ++ ", [I]"

decode :: Decoded
decode h = op (left h) (right h)

op :: Decoded'
op 0x0 h l = op0 h l
op 0x1 h l = Just . Jp $ merge h l
op 0x2 h l = Just . Call $ merge h l
op 0x3 h l = Just $ SeB h l
op 0x4 h l = Just $ SneB h l
op 0x5 h l = op5 h (left l) (right l)
op 0x6 h l = Just $ LdB h l
op 0x7 h l = Just $ AddB h l
op 0x8 h l = op8 h (left l) (right l)
op 0x9 h l = op9 h (left l) (right l)
op 0xA h l = Just . LdI $ merge h l
op 0xB h l = Just . JpV $ merge h l
op 0xC h l = Just $ Rnd h l
op 0xD h l = Just $ Drw h (left l) (right l)
op 0xE h l = opE h l
op 0xF h l = opF h l
op _ _ _ = Nothing

op0 :: Decoded
op0 0x0 0xE0 = Just Cls
op0 0x0 0xEE = Just Ret
op0 h l = Just . Sys $ merge h l

op5 :: Decoded'
op5 x y 0x0 = Just $ Se x y
op5 _ _ _ = Nothing

op8 :: Decoded'
op8 x y 0x0 = Just $ Ld x y
op8 x y 0x1 = Just $ Or x y
op8 x y 0x2 = Just $ And x y
op8 x y 0x3 = Just $ Xor x y
op8 x y 0x4 = Just $ Add x y
op8 x y 0x5 = Just $ Sub x y
op8 x y 0x6 = Just $ Shr x y
op8 x y 0x7 = Just $ Subn x y
op8 x y 0xE = Just $ Shl x y
op8 _ _ _ = Nothing

op9 :: Decoded'
op9 x y 0x0 = Just $ Sne x y
op9 _ _ _ = Nothing

opE :: Decoded
opE x 0x9E = Just $ Skp x
opE x 0xA1 = Just $ Sknp x
opE _ _ = Nothing

opF :: Decoded
opF x 0x07 = Just $ LdVDT x
opF x 0x0A = Just $ LdK x
opF x 0x15 = Just $ LdDTV x
opF x 0x18 = Just $ LdST x
opF x 0x1E = Just $ AddI x
opF x 0x29 = Just $ LdFV x
opF x 0x33 = Just $ LdBV x
opF x 0x55 = Just $ LdIV x
opF x 0x65 = Just $ LdVI x
opF _ _ = Nothing

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