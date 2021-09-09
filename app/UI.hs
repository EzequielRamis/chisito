module UI where

import Data.Bits.Bitwise (toListBE)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as V
import Foreign.C.Types (CInt)
import Game (Game (_display))
import SDL hiding (Timer)
import Utils

type Color = V4 Byte

render :: Game -> Renderer -> IO ()
render g r = do
  clean r
  d <- V.freeze $ _display g
  let pixelRows = zipWith pixelLines (V.toList d) [0, 1 ..]
      pixels = VS.concat pixelRows
  rendererDrawColor r $= foreground
  fillRects r pixels
  present r

clean :: Renderer -> IO ()
clean r = do
  rendererDrawColor r $= background
  clear r

pixelLines :: PixelRow -> CInt -> VS.Vector (Rectangle CInt)
pixelLines r y = VS.fromList $ rowToRects (toListBE r) y

rowToRects :: [Bool] -> CInt -> [Rectangle CInt]
rowToRects bs y =
  map (\(x, _) -> Rectangle (P $ V2 x y * pure size) (pure size)) $
    filter snd $ zip [0, 1 ..] bs

size :: CInt
size = 10

background :: Color
background = V4 0 0 0 255

foreground :: Color
foreground = V4 255 255 255 255