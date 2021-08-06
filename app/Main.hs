module Main where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Opcodes (Program, swapEnd)
import Text.Printf (printf)

second :: Int
second = 1000000

evalsPerSec :: Int -> Int
evalsPerSec = div second

hex :: String
hex = "%x"

main :: IO ()
main = do
  bytecode <- B.readFile "TETRIS"
  print $ program bytecode

program :: B.ByteString -> Program
program = swapEnd . B.unpack

load :: [Word8] -> IO ()
load [] = return ()
load (p : ps) = do
  threadDelay $ evalsPerSec 700
  putStrLn $ printf hex p
  load ps

-- toProgram bs = map toOpcode $ B.unpack bs
