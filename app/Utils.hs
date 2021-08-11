module Utils where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString.Builder (toLazyByteString, word16BE)
import Data.ByteString.Lazy (unpack)
import Text.Printf (printf)
import Types (Addr, Byte)
import Prelude hiding (max)

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
