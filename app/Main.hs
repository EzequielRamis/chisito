module Main where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import Decode (decode)
import Helpers (Program, merge, show2, show4, swapEnd)

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

load :: Program -> IO ()
load [] = return ()
load [b] = print $ "Invalid Opcode: 00" ++ show2 b
load (b : b' : bs) = do
  threadDelay $ ticksPerSec 700
  _ <- case opcode of
    Just op -> print $ show4 (merge b b') ++ ": " ++ show op
    Nothing -> print $ show4 (merge b b') ++ ": Nothing"
  load bs
  where
    opcode = decode b b'