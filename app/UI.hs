{-# LANGUAGE NamedFieldPuns #-}

module UI where

import Config
import Data.Bits.Bitwise (toListBE)
import Data.Int (Int32)
import Data.Maybe (fromJust)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as V
import Foreign.C.Types (CInt)
import Game (Game (_display))
import Lens.Micro ((^.))
import SDL
  ( Point (P),
    R1 (_x),
    R2 (_y),
    Rectangle (..),
    Renderer,
    V2 (..),
    V4 (V4),
    clear,
    fillRects,
    present,
    rendererDrawColor,
    ($=),
  )
import Types
import Utils

render :: V2 Int32 -> Game -> GameConfig -> Renderer -> IO ()
render v g c r = do
  clean c r
  d <- V.freeze $ _display g
  let pixelSize = getPixelSize v
      origin = getOrigin v pixelSize
      pixelRows = zipWith (pixelLines (pixelSize, origin)) (V.toList d) [0, 1 ..]
      pixels = VS.concat pixelRows
  rendererDrawColor r $= rgb (fromJust $ foreground c)
  fillRects r pixels
  present r

clean :: GameConfig -> Renderer -> IO ()
clean c r = do
  rendererDrawColor r $= rgb (fromJust $ background c)
  clear r

pixelLines :: (CInt, V2 CInt) -> PixelRow -> CInt -> VS.Vector (Rectangle CInt)
pixelLines s r y = VS.fromList $ rowToRects (toListBE r) s y

rowToRects :: [Bool] -> (CInt, V2 CInt) -> CInt -> [Rectangle CInt]
rowToRects bs (size, origin) y =
  map (\(x, _) -> Rectangle (P $ (V2 x y * pure size) + origin) (pure size)) $
    filter snd $ zip [0, 1 ..] bs

rgb :: RGB -> Color
rgb RGB {red, green, blue} = V4 red green blue 255

getPixelSize :: V2 Int32 -> CInt
getPixelSize v = fromIntegral $ min (div (v ^. _x) width) (div (v ^. _y) height)

getOrigin :: V2 Int32 -> CInt -> V2 CInt
getOrigin v s = fmap (`div` 2) (v' - (V2 width height * pure s))
  where
    v' = fmap fromIntegral v
