module Main where

import DataTypes
import SceneParser
import Scene
import qualified Data.ByteString as BStr
import Codec.BMP

imageToBmp :: Image -> BMP
imageToBmp (Image w h c) =
    packRGBA32ToBMP (w+1) (h+1). BStr.pack $ concatMap toWordList c

main :: IO ()
main = do
    scene <- parse
    let img = render scene
    print $ imWidth img
    print $ imHeight img
    print $ length $ imPixels img
    writeBMP "output.bmp" $ imageToBmp img
