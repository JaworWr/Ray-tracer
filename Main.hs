module Main where

import DataTypes
import SceneParser
import Scene
import qualified Data.ByteString as BStr
import System.Environment
import System.IO.Error
import Data.List
import Codec.BMP

imageToBmp :: Image -> BMP
imageToBmp (Image w h c) =
    packRGBA32ToBMP w h . BStr.pack $ concatMap toWordList c

changeExt :: String -> String
changeExt s = case dropWhileEnd (/= '.') s of
    [] -> s ++ ".bmp"
    s' -> s' ++ "bmp"

main :: IO ()
main = do
    args <- getArgs
    if null args
    then putStrLn "No input file specified"
    else tryIOError (readFile $ head args) >>=
        either print
            (either putStrLn
                (writeBMP (changeExt $ head args) . imageToBmp . render)
                . parse)
