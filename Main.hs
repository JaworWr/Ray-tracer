{-# LANGUAGE LambdaCase #-}

module Main where

import DataTypes
import SceneParser
import SceneValidator
import SceneRenderer
import Data.Maybe
import qualified Data.ByteString as BStr
import Control.Monad.Except
import System.Environment
import System.IO.Error
import System.Console.GetOpt
import Data.List
import Codec.BMP
import qualified Graphics.Gloss as G

data Options = Options {
    savePath :: Maybe String,
    saveImg :: Bool,
    showImg :: Bool,
    showHelp :: Bool
} deriving (Eq, Show)

defaultOpts :: Options
defaultOpts = Options {
    savePath = Nothing,
    saveImg = True,
    showImg = False,
    showHelp = False
}

options :: [OptDescr (Options -> Options)]
options = [
    Option ['n'] ["no-save"]
        (NoArg $ \opt -> opt { saveImg = False })
        "Do not save the image",
    Option ['o'] ["output-name"]
        (ReqArg (\s opt -> opt { savePath = Just s }) "name")
        "Output file name",
    Option ['s'] ["show"]
        (NoArg $ \opt -> opt { showImg = True })
        "Show the image after rendering",
    Option ['h'] ["help"]
        (NoArg $ \opt -> opt { showHelp = True })
        "Show this help message"
    ]

-- instrukcja użycia programu wyświetlana użytkownikowi
usageStr :: String -> String
usageStr progName = usageInfo header options where
    header = "Usage: " ++ progName ++ " [OPTION...] file"

-- funkcja parsująca opcje
parseOptions :: [String] -> Either String (Options, Maybe String)
parseOptions args = parsed where
    (fs, rs, errs) = getOpt Permute options args
    opts = foldl (flip id) defaultOpts fs
    path = fst <$> uncons rs
    parsed = case errs of
        [] -> return (opts, path)
        _ -> throwError $ unlines errs ++ "\n"

-- funkcja przekształcająca obraz do formatu BMP
imageToBmp :: Color t => Image t -> BMP
imageToBmp img = packRGBA32ToBMP (imWidth img) (imHeight img)
    . BStr.pack
    $ concatMap toWordList (imPixels img)

-- funkcja tworząca nazwę wyjściowego pliku na podstawie nazwy pliku wejściowego
changeExt :: String -> String
changeExt s = case dropWhileEnd (/= '.') s of
    [] -> s ++ ".bmp"
    s' -> s' ++ "bmp"

-- zwraca listę operacji do przeprowadzenia na obrazie
optionsToBmpOps :: String -> Options -> [BMP -> IO ()]
optionsToBmpOps fname opts = saveOp ++ showOp where
    saveName = fromMaybe (changeExt fname) $ savePath opts
    saveOp = [writeBMP saveName | saveImg opts]
    showOp = [showImage fname | showImg opts]

-- uruchamia wszystkie zadane operacje na obrazie
runBmpOps :: [BMP -> IO ()] -> BMP -> IO ()
runBmpOps l bmp = mapM_ ($ bmp) l

-- funkcja wyświetlająca utworzony obraz
-- oraz zapisująca go w formacie bmp
showImage :: String -> BMP -> IO ()
showImage name bmp = G.display
    (G.InWindow name (bmpDimensions bmp) (0, 0))
    G.black
    (G.bitmapOfBMP bmp)

-- pomocnicza funkcja przekształcająca typ Maybe na instancję MonadError
maybeThrow :: MonadError e m => e -> Maybe a -> m a
maybeThrow e = maybe (throwError e) return

-- główna funkcja obsługująca logikę programu
run :: ExceptT String IO ()
run = do
    progName <- lift getProgName
    (opts, path) <- withExceptT (++ usageStr progName)
        $ lift getArgs >>= liftEither . parseOptions
    if showHelp opts
        then lift
            . putStrLn
            $ usageStr progName
        else do
            path <- maybeThrow
                ("No input file specified.\n\n" ++ usageStr progName)
                path
            s <- withExceptT show
                . ExceptT
                . tryIOError
                . readFile
                $ path
            scene <- withExceptT show
                . liftEither
                $ parseScene path s
            validated <- liftEither $ validateScene scene
            lift
                . runBmpOps (optionsToBmpOps path opts)
                . imageToBmp
                . renderValidated
                $ validated

main :: IO ()
main = runExceptT run >>= \case
    Left e -> putStrLn e
    _ -> return ()
