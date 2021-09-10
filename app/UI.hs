module UI where

import Data.Bits.Bitwise (toListBE)
import Data.Int (Int32)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as V
import Foreign.C.Types (CInt)
import Game (Game (_display))
import Lens.Micro ((^.))
import SDL hiding (Timer, origin)
import Utils

type Color = V4 Byte

render :: V2 Int32 -> Game -> Renderer -> IO ()
render v g r = do
  clean r
  d <- V.freeze $ _display g
  let pixelSize = getPixelSize v
      origin = getOrigin v pixelSize
      pixelRows = zipWith (pixelLines (pixelSize, origin)) (V.toList d) [0, 1 ..]
      pixels = VS.concat pixelRows
  rendererDrawColor r $= foreground
  fillRects r pixels
  present r

clean :: Renderer -> IO ()
clean r = do
  rendererDrawColor r $= background
  clear r

pixelLines :: (CInt, V2 CInt) -> PixelRow -> CInt -> VS.Vector (Rectangle CInt)
pixelLines s r y = VS.fromList $ rowToRects (toListBE r) s y

rowToRects :: [Bool] -> (CInt, V2 CInt) -> CInt -> [Rectangle CInt]
rowToRects bs (size, origin) y =
  map (\(x, _) -> Rectangle (P $ (V2 x y * pure size) + origin) (pure size)) $
    filter snd $ zip [0, 1 ..] bs

background :: Color
background = V4 0 0 0 255

foreground :: Color
foreground = V4 255 255 255 255

getPixelSize :: V2 Int32 -> CInt
getPixelSize v = fromIntegral $ min (div (v ^. _x) width) (div (v ^. _y) height)

getOrigin :: V2 Int32 -> CInt -> V2 CInt
getOrigin v s = fmap (`div` 2) (v' - (V2 width height * pure s))
  where
    v' = fmap fromIntegral v
