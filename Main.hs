module Main where

import DataTypes
import SceneParser
import Scene
import qualified Data.ByteString as BStr
import System.Environment
import System.IO.Error
import Data.List
import Codec.BMP
import qualified Graphics.Gloss as G

-- funkcja przekształcająca obraz do formatu BMP
imageToBmp :: Color t => Image t -> BMP
imageToBmp (Image w h c) =
    packRGBA32ToBMP w h . BStr.pack $ concatMap toWordList c

-- funkcja tworząca nazwę wyjściowego pliku na podstawie nazwy pliku wejściowego
changeExt :: String -> String
changeExt s = case dropWhileEnd (/= '.') s of
    [] -> s ++ ".bmp"
    s' -> s' ++ "bmp"

showImage :: Color t => String -> Scene t -> IO ()
showImage name s = do
    let bmp = imageToBmp $ render s
    writeBMP (changeExt name) bmp
    G.display (G.InWindow name (pxWidth s, pxHeight s) (0, 0))
        G.black (G.bitmapOfBMP bmp)

main :: IO ()
main = do
    args <- getArgs
    if null args
    then putStrLn "No input file specified"
    else tryIOError (readFile $ head args) >>=
        either print (either putStrLn (showImage $ head args). parseScene (head args))
