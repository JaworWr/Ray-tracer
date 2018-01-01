module DataTypes where

import Data.Word

-- typ danych reprezentujący wektor w przestrzeni trójwymiarowej
data Vector = Vector Double Double Double deriving (Show, Eq)

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

-- typ danych reprezentujący kolory
data Color =
    RGB Double Double Double
    deriving (Eq, Show)

-- funkcja tworząca kolory w postaci RGB
makeRGB :: Double -> Double -> Double -> Color
makeRGB r g b = RGB (norm r) (norm g) (norm b) where
    norm = min 1 . max 0

-- zamiana koloru na listę czterech wartości typu Word8
toWordList :: Color -> [Word8]
toWordList = map (round . (* 255)) . toDoubleList where
    toDoubleList (RGB r g b) = [r, g, b, 1]

-- mnożenie koloru przez liczbę (z przedziału [0, 1])
infixl 7 `cMult`
cMult :: Double -> Color -> Color
cMult t (RGB r g b) = RGB (t * r) (t * g) (t * b)

-- stałe reprezentujące wybrane kolory
black :: Color
black = RGB 0 0 0
