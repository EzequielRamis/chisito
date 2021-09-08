module UI where

import Data.Bits.Bitwise (toListBE)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as V
import Decode (Opcode (Cls, Drw))
import Foreign.C.Types (CInt)
import Game (Game (_display))
import SDL hiding (Timer)
import Utils

type Color = V4 Byte

render :: Opcode -> Game -> Renderer -> IO ()
render Cls _ r = do
  clean r
  present r
render Drw {} g r = do
  clean r
  rendererDrawColor r $= foreground
  fillRects r pixels
  present r
  where
    d = _display g
    pixelRows = zipWith pixelLines (V.toList d) [0, 1 ..]
    pixels = VS.concat pixelRows
render _ _ _ = return ()

clean :: Renderer -> IO ()
clean r = do
  rendererDrawColor r $= background
  clear r

pixelLines :: PixelRow -> CInt -> VS.Vector (Rectangle CInt)
pixelLines r h = VS.fromList $ rowToRects (toListBE r) h

rowToRects :: [Bool] -> CInt -> [Rectangle CInt]
rowToRects bs h =
  map (\(x, _) -> Rectangle (P $ V2 (x * size) (h * size)) (pure size)) $
    filter snd $ zip [0, 1 ..] bs

size :: CInt
size = 10

background :: Color
background = V4 0 0 0 255

foreground :: Color
foreground = V4 255 255 255 255