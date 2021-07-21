module Opcodes where

import Data.Bits (Bits (shiftR, (.&.)))
import Data.Word (Word8)

type Nibble = Word8

type Opcode = (Nibble, Nibble, Nibble, Nibble)

type Addr = (Nibble, Nibble, Nibble)

type Byte = (Nibble, Nibble)

toOpcode :: [Nibble] -> Opcode
toOpcode [b, a] = (big a, low a, big b, low b)
toOpcode _ = error "Invalid ROM Format"

big :: Nibble -> Nibble
big n = shiftR (maskB .&. n) 0x4

low :: Nibble -> Nibble
low n = maskL .&. n

maskB :: Nibble
maskB = 0xF0

maskL :: Nibble
maskL = 0x0F

-- Opcodes --

execute :: Opcode -> ()
execute (0x0, 0x0, 0xE, 0xE) = () -- RET
execute (0x0, 0x0, 0xE, 0x0) = () -- CLS
execute (0x0, _n2, _n1, _n0) = () -- SYS addr
execute (0x1, _n2, _n1, _n0) = () -- JP addr
execute (0x2, _n2, _n1, _n0) = () -- CALL addr
execute (0x3, _x0, _k1, _k0) = () -- SE Vx, byte
execute (0x4, _x0, _k1, _k0) = () -- SNE Vx, byte
execute (0x5, _x0, _y0, 0x0) = () -- SE Vx, Vy
execute (0x6, _x0, _k1, _k0) = () -- LD Vx, byte
execute (0x7, _x0, _k1, _k0) = () -- ADD Vx, byte
execute (0x8, _x0, _y0, 0x0) = () -- LD Vx, Vy
execute (0x8, _x0, _y0, 0x1) = () -- OR Vx, Vy
execute (0x8, _x0, _y0, 0x2) = () -- AND Vx, Vy
execute (0x8, _x0, _y0, 0x3) = () -- XOR Vx, Vy
execute (0x8, _x0, _y0, 0x4) = () -- ADD Vx, Vy
execute (0x8, _x0, _y0, 0x5) = () -- SUB Vx, Vy
execute (0x8, _x0, _y0, 0x6) = () -- SHR Vx {, Vy}
execute (0x8, _x0, _y0, 0x7) = () -- SUBN Vx, Vy
execute (0x8, _x0, _y0, 0xE) = () -- SHL Vx {, Vy}
execute (0x9, _x0, _y0, 0x0) = () -- SNE Vx, Vy
execute (0xA, _n2, _n1, _n0) = () -- LD I, addr
execute (0xB, _n2, _n1, _n0) = () -- JP V0, addr
execute (0xC, _x0, _k1, _k0) = () -- RND Vx, byte
execute (0xD, _x0, _y0, _n0) = () -- DRW Vx, Vy, nibble
execute (0xE, _x0, 0x9, 0xE) = () -- SKP Vx
execute (0xE, _x0, 0xA, 0x1) = () -- SKNP Vx
execute (0xF, _x0, 0x0, 0x7) = () -- LD Vx, DT
execute (0xF, _x0, 0x0, 0xA) = () -- LD Vx, K
execute (0xF, _x0, 0x1, 0x5) = () -- LD DT, Vx
execute (0xF, _x0, 0x1, 0x8) = () -- LD ST, Vx
execute (0xF, _x0, 0x1, 0xE) = () -- ADD I, Vx
execute (0xF, _x0, 0x2, 0x9) = () -- LD F, Vx
execute (0xF, _x0, 0x3, 0x3) = () -- LD B, Vx
execute (0xF, _x0, 0x5, 0x5) = () -- LD [I], Vx
execute (0xF, _x0, 0x6, 0x5) = () -- LD Vx, [I]
execute _ = () -- error