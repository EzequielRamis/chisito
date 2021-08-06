import Data.Vector.Mutable
import Data.Word (Word16, Word8)

type Memory = STVector Word8

data Game = Game
  { pc :: Word16,
    sp :: Word8,
    memory :: [Word8],
    stack :: [Word16],
    v0 :: Word8,
    v1 :: Word8,
    v2 :: Word8,
    v3 :: Word8,
    v4 :: Word8,
    v5 :: Word8,
    v6 :: Word8,
    v7 :: Word8,
    v8 :: Word8,
    v9 :: Word8,
    vA :: Word8,
    vB :: Word8,
    vC :: Word8,
    vD :: Word8,
    vE :: Word8,
    vF :: Word8
  }