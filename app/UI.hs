module UI (Display, refresh) where

import Data.Bits.Bitwise (toListBE)
import Data.List (group)
import qualified Data.Vector.Unboxed as V
import Decode (Opcode (Cls, Drw))
import Game (Display, Game (..))
import Graphics.Vty
  ( Image,
    Vty (update),
    black,
    defAttr,
    emptyImage,
    horizCat,
    picForImage,
    string,
    vertCat,
    white,
    withBackColor,
  )
import Utils (PixelRow)

refresh :: Vty -> Game -> Opcode -> IO ()
refresh t g Drw {} = draw (_display g) t
refresh t _ Cls = return ()
refresh _ _ _ = return ()

draw :: Display -> Vty -> IO ()
draw d v = update v pic
  where
    pixelRows = map pixelLines' $ V.toList d
    img = vertCat pixelRows
    pic = picForImage img

pixelLines :: PixelRow -> Image
pixelLines r = horizCat $ map pixelLine $ group $ toListBE r

pixelLines' :: PixelRow -> Image
pixelLines' r = horizCat $ map pixel $ toListBE r

pixelLine :: [Bool] -> Image
pixelLine b@(True : _) = string (defAttr `withBackColor` white) $ concat $ replicate (length b) pixelStr
pixelLine b@(False : _) = string (defAttr `withBackColor` black) $ concat $ replicate (length b) pixelStr
pixelLine _ = emptyImage

pixelStr :: String
pixelStr = "  "

pixel :: Bool -> Image
pixel True = string (defAttr `withBackColor` white) pixelStr
pixel False = string (defAttr `withBackColor` black) pixelStr