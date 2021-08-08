{-# LANGUAGE TemplateHaskell #-}

module Game where

import Data.Bifunctor (first)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.Vector.Unboxed as V
import Decode (Opcode (..))
import Helpers (Addr, Byte, Err, Stack, new, pop, push)
import Lens.Micro.Platform
import System.Random (StdGen, newStdGen, randomR)

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
    _keyboard :: Keyboard,
    _seed :: StdGen
  }

clear :: Display
clear = V.replicate (32 * 64) False

fetch :: (V.Unbox a) => V.Vector a -> Byte -> a
fetch v b = v V.! fromIntegral b

(//) :: (V.Unbox a) => V.Vector a -> [(Byte, a)] -> V.Vector a
(//) a b = a // map (first fromIntegral) b

update :: (V.Unbox a) => V.Vector a -> V.Vector a -> V.Vector a
update a b = V.update_ a (V.generate (V.length b) id) b

newGame :: IO Game
newGame =
  do
    s <- newStdGen
    return
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
          _keyboard = V.replicate 16 False,
          _seed = s
        }

makeLenses ''Game

exec :: Opcode -> Game -> Either Err Game
--

-- Clear the display
exec Cls g = return $ g & set display clear
--

-- Return from a subroutine
exec Ret g = do
  (s, a) <- pop $ _stack g
  return $
    g & over sp (\p -> p - 1)
      & set stack s
      & set pc a

--

-- Jump to location addr
exec (Jp addr) g = return $ g & set pc addr
--

-- Call subroutine at addr
exec (Call addr) g = do
  s <- _pc g `push` _stack g
  return $
    g & over sp (+ 1)
      & set stack s
      & set pc addr
--

-- Skip next instruction if Vx = byte
exec (SeB vx b) g
  | x == b = return $ g & over pc (+ 2)
  | otherwise = return g
  where
    x = _registers g `fetch` vx
--

-- Skip next instruction if Vx != byte
exec (SneB vx b) g
  | x /= b = return $ g & over pc (+ 2)
  | otherwise = return g
  where
    x = _registers g `fetch` vx
--

-- Skip next instruction if Vx = Vy
exec (Se vx vy) g
  | x == y = return $ g & over pc (+ 2)
  | otherwise = return g
  where
    x = _registers g `fetch` vx
    y = _registers g `fetch` vy
--

-- Set Vx = byte
exec (LdB vx b) g = return $ g & over registers (// [(vx, b)])
--

-- Set Vx = Vx + kk
exec (AddB vx b) g = return $ g & over registers (// [(vx, x + b)])
  where
    x = _registers g `fetch` vx
--

-- Set Vx = Vy
exec (Ld vx vy) g = return $ g & over registers (// [(vx, y)])
  where
    y = _registers g `fetch` vy
--

-- Set Vx = Vx OR Vy
exec (Or vx vy) g = return $ g & over registers (// [(vx, z)])
  where
    x = _registers g `fetch` vx
    y = _registers g `fetch` vy
    z = x .|. y
--

-- Set Vx = Vx AND Vy
exec (And vx vy) g = return $ g & over registers (// [(vx, z)])
  where
    x = _registers g `fetch` vx
    y = _registers g `fetch` vy
    z = x .&. y
--

-- Set Vx = Vx XOR Vy
exec (Xor vx vy) g = return $ g & over registers (// [(vx, z)])
  where
    x = _registers g `fetch` vx
    y = _registers g `fetch` vy
    z = xor x y
--

-- Set Vx = Vx + Vy, set VF = carry
exec (Add vx vy) g = return $ g & over registers (// [(vx, z), (0xF, carry)])
  where
    x = _registers g `fetch` vx
    y = _registers g `fetch` vy
    z = x + y
    carry = if z > 0xFF then 1 else 0 :: Byte
--

-- Set Vx = Vx - Vy, set VF = NOT borrow
exec (Sub vx vy) g = return $ g & over registers (// [(vx, z), (0xF, borrow)])
  where
    x = _registers g `fetch` vx
    y = _registers g `fetch` vy
    z = x - y
    borrow = if x > y then 1 else 0 :: Byte
--

-- Set Vx = Vx SHR 1 (https://tobiasvl.github.io/blog/write-a-chip-8-emulator/#8xy6-and-8xye-shift)
exec (Shr vx) g = return $ g & over registers (// [(vx, d), (0xF, z)])
  where
    x = _registers g `fetch` vx
    z = if x .&. 0x0F == 0x01 then 1 else 0
    d = shiftR x 1
--

-- Set Vx = Vy - Vx, set VF = NOT borrow
exec (Subn vx vy) g = return $ g & over registers (// [(vx, z), (0xF, borrow)])
  where
    x = _registers g `fetch` vx
    y = _registers g `fetch` vy
    z = y - x
    borrow = if y > x then 1 else 0 :: Byte
--

-- Set Vx = Vx SHL 1 (https://tobiasvl.github.io/blog/write-a-chip-8-emulator/#8xy6-and-8xye-shift)
exec (Shl vx) g = return $ g & over registers (// [(vx, d), (0xF, z)])
  where
    x = _registers g `fetch` vx
    z = if x .&. 0xF0 == 0x10 then 1 else 0
    d = shiftL x 1
--

-- Skip next instruction if Vx != Vy
exec (Sne vx vy) g
  | x /= y = return $ g & over pc (+ 2)
  | otherwise = return g
  where
    x = _registers g `fetch` vx
    y = _registers g `fetch` vy
--

-- Set I = addr
exec (LdI addr) g = return $ g & set i addr
--

-- Jump to location addr + V0
exec (JpV addr) g = return $ g & set pc (addr + v)
  where
    v = fromIntegral $ _registers g `fetch` 0x0 :: Addr
--

-- Set Vx = random byte AND byte
exec (Rnd vx b) g =
  return $
    g & over registers (// [(vx, z)])
      & set seed gen
  where
    (rnd, gen) = randomR (0x0 :: Byte, 0xFF :: Byte) (_seed g)
    z = rnd .&. b
--
exec _ _ = undefined