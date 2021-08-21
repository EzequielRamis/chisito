{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Game (newGame, exec, Game (..), Timer, fetch, next) where

import Control.Concurrent.MVar
import Data.Bifunctor (first)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Bits.Bitwise (toListBE)
import Data.List (group)
import qualified Data.Vector.Unboxed as V
import Data.Word (byteSwap64)
import Decode (Opcode (..))
import Graphics.Vty
import Lens.Micro.Platform
import System.Random (randomRIO)
import Utils

type Registers = V.Vector Byte

type Memory = V.Vector Byte

type Display = V.Vector PixelRow

type Timer = MVar Byte

data Game = Game
  { _pc :: Addr,
    _sp :: Byte,
    _i :: Addr,
    _dt :: Timer,
    _st :: Timer,
    _memory :: Memory,
    _stack :: Stack Addr,
    _registers :: Registers,
    _display :: Display
    -- _tui :: Vty
  }
  deriving (Show)

instance Show (MVar a) where
  show _ = ""

instance Show Vty where
  show _ = ""

blankDisplay :: Display
blankDisplay = V.replicate 32 0x0

regVal :: (V.Unbox a) => Byte -> V.Vector a -> a
regVal b v = v V.! fromIntegral b

(//) :: (V.Unbox a) => V.Vector a -> [(Byte, a)] -> V.Vector a
(//) a b = a V.// map (first fromIntegral) b

newGame :: Config -> Program -> IO Game
newGame c p =
  do
    d <- newMVar 0x0
    s <- newMVar 0x0
    -- t <- mkVty c
    return
      Game
        { _pc = 0x200,
          _sp = 0x0,
          _i = 0x0,
          _dt = d,
          _st = s,
          _memory =
            V.replicate 4096 0x0
              V.// ( zip [0x200 :: Int ..] p
                       ++ zip [0x50 ..] font
                   ),
          _stack = newStack 16,
          _registers = V.replicate 16 0x0,
          _display = blankDisplay
          -- _tui = t
        }

makeLenses ''Game

fetch :: Game -> [Byte]
fetch g = (V.toList . V.take 2 . V.drop p) mem
  where
    mem = g ^. memory
    p = fromIntegral $ g ^. pc

returnG :: Game -> IO (Either Err Game)
returnG = return . return

returnE :: Err -> IO (Either Err Game)
returnE = return . Left

next :: Game -> Game
next g = g & over pc (+ 2)

exec :: Opcode -> Game -> IO (Either Err Game)
--

-- Jump to a machine code routine at nnn (IGNORE)
exec (Sys _) g = returnG g
--

-- Clear the display
exec Cls g = do
  -- update (g ^. tui) $ picForImage $ backgroundFill 64 32
  returnG $ g & set display blankDisplay
--

-- Return from a subroutine
exec Ret g = do
  case pop $ g ^. stack of
    Left e -> returnE e
    Right (s, a) ->
      returnG $
        g & over sp (\p -> p - 1)
          & set stack s
          & set pc a
--

-- Jump to location addr
exec (Jp addr) g = returnG $ g & set pc addr
--

-- Call subroutine at addr
exec (Call addr) g = do
  case push (g ^. pc) (g ^. stack) of
    Left e -> returnE e
    Right s ->
      returnG $
        g & over sp (+ 1)
          & set stack s
          & set pc addr
--

-- Skip next instruction if Vx = byte
exec (SeB vx b) g
  | x == b = returnG $ next g
  | otherwise = returnG g
  where
    x = regVal vx $ g ^. registers
--

-- Skip next instruction if Vx != byte
exec (SneB vx b) g
  | x /= b = returnG $ next g
  | otherwise = returnG g
  where
    x = regVal vx $ g ^. registers
--

-- Skip next instruction if Vx = Vy
exec (Se vx vy) g
  | x == y = returnG $ next g
  | otherwise = returnG g
  where
    x = regVal vx $ g ^. registers
    y = regVal vy $ g ^. registers
--

-- Set Vx = byte
exec (LdB vx b) g = returnG $ g & over registers (// [(vx, b)])
--

-- Set Vx = Vx + kk
exec (AddB vx b) g = returnG $ g & over registers (// [(vx, x + b)])
  where
    x = regVal vx $ g ^. registers
--

-- Set Vx = Vy
exec (Ld vx vy) g = returnG $ g & over registers (// [(vx, y)])
  where
    y = regVal vy $ g ^. registers
--

-- Set Vx = Vx OR Vy
exec (Or vx vy) g = returnG $ g & over registers (// [(vx, z)])
  where
    x = regVal vx $ g ^. registers
    y = regVal vy $ g ^. registers
    z = x .|. y
--

-- Set Vx = Vx AND Vy
exec (And vx vy) g = returnG $ g & over registers (// [(vx, z)])
  where
    x = regVal vx $ g ^. registers
    y = regVal vy $ g ^. registers
    z = x .&. y
--

-- Set Vx = Vx XOR Vy
exec (Xor vx vy) g = returnG $ g & over registers (// [(vx, z)])
  where
    x = regVal vx $ g ^. registers
    y = regVal vy $ g ^. registers
    z = xor x y
--

-- Set Vx = Vx + Vy, set VF = carry
exec (Add vx vy) g = returnG $ g & over registers (// [(vx, z), (0xF, carry)])
  where
    x = regVal vx $ g ^. registers
    y = regVal vy $ g ^. registers
    z = x + y
    carry = if z > 0xFF then 1 else 0 :: Byte
--

-- Set Vx = Vx - Vy, set VF = NOT borrow
exec (Sub vx vy) g = returnG $ g & over registers (// [(vx, z), (0xF, borrow)])
  where
    x = regVal vx $ g ^. registers
    y = regVal vy $ g ^. registers
    z = x - y
    borrow = if x > y then 1 else 0 :: Byte
--

-- Set Vx = Vx SHR 1 (https://tobiasvl.github.io/blog/write-a-chip-8-emulator/#8xy6-and-8xye-shift)
exec (Shr vx) g = returnG $ g & over registers (// [(vx, d), (0xF, z)])
  where
    x = regVal vx $ g ^. registers
    z = if x .&. 0x0F == 0x01 then 1 else 0
    d = shiftR x 1
--

-- Set Vx = Vy - Vx, set VF = NOT borrow
exec (Subn vx vy) g = returnG $ g & over registers (// [(vx, z), (0xF, borrow)])
  where
    x = regVal vx $ g ^. registers
    y = regVal vy $ g ^. registers
    z = y - x
    borrow = if y > x then 1 else 0 :: Byte
--

-- Set Vx = Vx SHL 1 (https://tobiasvl.github.io/blog/write-a-chip-8-emulator/#8xy6-and-8xye-shift)
exec (Shl vx) g = returnG $ g & over registers (// [(vx, d), (0xF, z)])
  where
    x = regVal vx $ g ^. registers
    z = if x .&. 0xF0 == 0x10 then 1 else 0
    d = shiftL x 1
--

-- Skip next instruction if Vx != Vy
exec (Sne vx vy) g
  | x /= y = returnG $ next g
  | otherwise = returnG g
  where
    x = regVal vx $ g ^. registers
    y = regVal vy $ g ^. registers
--

-- Set I = addr
exec (LdI addr) g = returnG $ g & set i addr
--

-- Jump to location addr + V0
exec (JpV addr) g = returnG $ g & set pc (addr + v)
  where
    v = fromIntegral $ regVal 0x0 $ g ^. registers
--

-- Set Vx = random byte AND byte
exec (Rnd vx b) g = do
  rnd <- randomRIO (0x0, 0xFF)
  let z = rnd .&. b
  returnG $
    g & over registers (// [(vx, z)])
--

-- Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision
exec (Drw vx vy nb) g = do
  -- draw newDisplay (g ^. tui)
  returnG $
    g & set display newDisplay
      & over registers (// [(0xF, collision)])
  where
    -- Wrapped coordinates
    x = fromIntegral $ mod (regVal vx $ g ^. registers) 64
    y = fromIntegral $ mod (regVal vy $ g ^. registers) 32
    -- Fetched sprite
    loc = fromIntegral $ g ^. i
    n = fromIntegral nb
    sprite = V.take n $ V.drop loc (g ^. memory)
    --
    oldDisplay = g ^. display
    -- Place sprite at x-coordinate
    rowMask = V.map (\r -> shiftR (byteSwap64 $ fromIntegral r) x) sprite
    -- Place sprite at y-coordinate and get mask
    ixs = V.generate n (+ y)
    displayMask = V.unsafeUpdate blankDisplay $ V.zip ixs rowMask
    -- If two bits are 1, it means they are collided
    zipDisplay = V.zip oldDisplay displayMask
    collided (old, new) = old .&. new > 0
    collision = if V.any collided zipDisplay then 1 else 0 :: Byte
    --
    newDisplay = V.zipWith xor oldDisplay displayMask
--

-- Skip next instruction if key with the value of Vx is pressed
-- exec (Skp vx) g = do
--   event <- nextEventNonblocking $ g ^. tui
--   case event of
--     Just (EvKey key _) -> do
--       let k = keyVal key defaultKeymap
--       case k of
--         Just b ->
--           if b == x
--             then skip
--             else continue
--         Nothing -> continue
--     _ -> continue
--   where
--     x = regVal vx $ g ^. registers
--     continue = returnG g
--     skip = returnG $ next g
--

-- Skip next instruction if key with the value of Vx is not pressed
-- exec (Sknp vx) g = do
--   event <- nextEventNonblocking $ g ^. tui
--   case event of
--     Just (EvKey key _) -> do
--       let k = keyVal key defaultKeymap
--       case k of
--         Just b ->
--           if b == x
--             then continue
--             else skip
--         Nothing -> skip
--     _ -> skip
--   where
--     x = regVal vx $ g ^. registers
--     continue = returnG g
--     skip = returnG $ next g
--

-- Set Vx = delay timer value
exec (LdVDT vx) g = do
  d <- readMVar (_dt g)
  returnG $ g & over registers (// [(vx, d)])
--

-- Wait for a key press, store the value of the key in Vx
-- exec (LdK vx) g = do
--   event <- nextEvent $ g ^. tui
--   case event of
--     EvKey key _ -> do
--       let k = keyVal key defaultKeymap
--       case k of
--         Just b -> returnG $ g & over registers (// [(vx, b)])
--         Nothing -> retry
--     _ -> retry
--   where
--     retry = exec (LdK vx) g
--

-- Set delay timer = Vx
exec (LdDTV vx) g = do
  _ <- swapMVar (g ^. dt) x
  returnG g
  where
    x = regVal vx $ g ^. registers
--

-- Set sound timer = Vx
exec (LdST vx) g = do
  _ <- swapMVar (g ^. st) x
  returnG g
  where
    x = regVal vx $ g ^. registers
--

-- Set I = I + Vx
exec (AddI vx) g = do
  returnG $ g & over i (+ x)
  where
    x = fromIntegral $ regVal vx $ g ^. registers
--

-- Set I = location of sprite for digit Vx
exec (LdFV vx) g = do
  returnG $ g & set i (fontStartAddr + fontHeight * digit)
  where
    digit = fromIntegral $ (.&. 0x0F) $ regVal vx $ g ^. registers
--

-- Store BCD representation of Vx in memory locations I, I+1, and I+2
exec (LdBV vx) g = do
  returnG $
    g
      & over
        memory
        ( V.//
            [ (loc, hnds),
              (loc + 1, tens),
              (loc + 2, ones)
            ]
        )
  where
    x = regVal vx $ g ^. registers
    hnds = div x 100
    tens = mod (div x 10) 10
    ones = mod x 10
    loc = fromIntegral $ g ^. i
--

-- Store registers V0 through Vx in memory starting at location I
exec (LdIV vx) g = do
  returnG $ g & over memory (`V.update` rgs)
  where
    len = fromIntegral vx + 1
    loc = fromIntegral $ g ^. i
    ixs = V.generate len (loc +)
    vls = V.take len (g ^. registers)
    rgs = V.zip ixs vls
--

-- Read registers V0 through Vx from memory starting at location I
exec (LdVI vx) g = do
  returnG $ g & over registers (`V.update` rgs)
  where
    len = fromIntegral vx + 1
    loc = fromIntegral $ g ^. i
    vls = V.take len $ V.drop loc (g ^. memory)
    rgs = V.indexed vls

--
exec _ g = returnG g

draw :: Display -> Vty -> IO ()
draw d v = update v pic
  where
    pixelRows = map pixelLines $ V.toList d
    img = vertCat pixelRows
    pic = picForImage img

pixelLines :: PixelRow -> Image
pixelLines r = horizCat $ map pixelLine $ group $ toListBE r

pixelLines' :: PixelRow -> Image
pixelLines' r = horizCat $ map pixel $ toListBE r

pixelLine :: [Bool] -> Image
pixelLine b@(True : _) = string (defAttr `withBackColor` white) $ concat $ replicate (length b) pixelStr
pixelLine b@(False : _) = backgroundFill (length b) 1
pixelLine _ = emptyImage

pixelStr :: String
pixelStr = "g "

pixel :: Bool -> Image
pixel True = string (defAttr `withBackColor` white) pixelStr
pixel False = backgroundFill 1 1