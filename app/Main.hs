module Main where

import qualified Data.ByteString as B
import Data.List.Split (divvy)
import Opcodes (Opcode, toOpcode)

main :: IO ()
main = do
  content <- B.readFile "TETRIS"
  print $ toProgram content

toProgram :: B.ByteString -> [Opcode]
toProgram bs = map toOpcode $ divvy 2 2 $ B.unpack bs
