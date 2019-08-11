{-# LANGUAGE LambdaCase #-}

module Main where

import DataTypes
import SceneParser
import Scene
import qualified Data.ByteString as BStr
import Control.Monad.Except
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

-- funkcja wyświetlająca utworzony obraz
-- oraz zapisująca go w formacie bmp
showImage :: Color t => String -> Image t -> IO ()
showImage name i = do
    let bmp = imageToBmp i
    writeBMP (changeExt name) bmp
    G.display (G.InWindow name (imWidth i, imHeight i) (0, 0))
        G.black (G.bitmapOfBMP bmp)

runRayTracer :: ExceptT String IO ()
runRayTracer = do
    args <- (fmap fst . uncons) <$> lift getArgs
    path <- maybe (throwError "No input file specified") return args
    s <- withExceptT show . ExceptT . tryIOError . readFile $ path
    scene <- liftEither $ parseScene path s
    lift . showImage path . render $ scene

main :: IO ()
main = runExceptT runRayTracer >>= \case
    Left e -> putStrLn e
    _ -> return ()
