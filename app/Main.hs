module Main where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Numeric (showHex)
import Opcodes (Program, decode, merge, swapEnd)

second :: Int
second = 1000000

ticksPerSec :: Int -> Int
ticksPerSec = div second

main :: IO ()
main = do
  bytecode <- B.readFile "TETRIS"
  load $ program bytecode

program :: B.ByteString -> Program
program = swapEnd . B.unpack

load :: [Word8] -> IO ()
load [] = return ()
load [b] = print $ "Invalid Opcode: 00" ++ showHex b ""
load (b : b' : bs) = do
  threadDelay $ ticksPerSec 700
  _ <- case opcode of
    Just op -> print $ showHex (merge b b') ": " ++ show op
    Nothing -> print $ showHex (merge b b') ": Nothing"
  load bs
  where
    opcode = decode b b'