{-# LANGUAGE DeriveGeneric #-}

module DataTypes where

import Data.Word
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- typ danych reprezentujący wektor w przestrzeni trójwymiarowej
data Vector = Vector Double Double Double deriving (Show, Eq, Generic)
instance NFData Vector

-- dodawanie wektorów
infixl 6 +.
(+.) :: Vector -> Vector -> Vector
(+.) (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

-- odejmowanie wektorów
infixl 6 -.
(-.) :: Vector -> Vector -> Vector
(-.) (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 - x2) (y1 - y2) (z1 - z2)

-- mnożenie wektora przez skalar
infixl 7 `times`
times :: Double -> Vector -> Vector
times t (Vector x y z) = Vector (t*x) (t*y) (t*z)

-- iloczyn skalarny
infixl 7 `dot`
dot :: Vector -> Vector -> Double
dot (Vector x1 y1 z1) (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

-- kwadrat długości wektora
sqVecLen :: Vector -> Double
sqVecLen v = v `dot` v

-- długość wektora
vecLen :: Vector -> Double
vecLen = sqrt . sqVecLen

-- funkcja normalizująca wektory
normalize :: Vector -> Vector
normalize v = (1 / vecLen v) `times` v

-- klasa reprezentująca kolory
class Color t where
    -- dodawanie kolorów
    infixl 6 `cAdd`
    cAdd :: t -> t -> t
    -- mnożenie koloru przez skalar
    infixl 7 `cTimes`
    cTimes :: Double -> t -> t
    -- mnożenie kolorów
    infixl 7 `cMult`
    cMult :: t -> t -> t
    -- zamiana koloru na listę 4 wartości reprezentujących kolor w formacie RGBA32
    toWordList :: t -> [Word8]

    -- stałe reprezentujące kolor czarny i biały
    black :: t
    white :: t

-- pomocnicza funkcja przekształcająca kanał na słowo 8-bitowe
channelToWord :: Double -> Word8
channelToWord = round . (* 255) . max 0 . min 1

-- typ danych reprezentujący odcienie szarości
type Greyscale = Double

instance Color Double where
    cAdd = (+)
    cTimes = (*)
    cMult = (*)
    toWordList x = [xw, xw, xw, 1] where
        xw = channelToWord x

    black = 0
    white = 1

-- typ danych reprezentujący kolory w formacie RGB
type RGB = Vector

-- konstruktor kolorów w formacie RGB
makeRGB :: Double -> Double -> Double -> RGB
makeRGB = Vector

instance Color Vector where
    cAdd = (+.)
    cTimes = times
    cMult (Vector r1 g1 b1) (Vector r2 g2 b2) = Vector (r1 * r2) (g1 * g2) (b1 * b2)
    toWordList = (++ [255]) . map channelToWord . toDoubleList where
         toDoubleList (Vector r g b) = [r, g, b]

    black = Vector 0 0 0
    white = Vector 1 1 1

-- stałe reprezentujące wybrane dodatkowe kolory
red :: RGB
red = makeRGB 1 0 0

green :: RGB
green = makeRGB 0 1 0

blue :: RGB
blue = makeRGB 0 0 1

cyan :: RGB
cyan = makeRGB 0 1 1

magenta :: RGB
magenta = makeRGB 1 0 1

yellow :: RGB
yellow = makeRGB 1 1 0
