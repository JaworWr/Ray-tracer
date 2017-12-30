module DataTypes where

import Data.Word

data Vector = Vector Double Double Double deriving (Show, Eq)

infixl 6 +.
(+.) :: Vector -> Vector -> Vector
(+.) (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

infixl 6 -.
(-.) :: Vector -> Vector -> Vector
(-.) (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 - x2) (y1 - y2) (z1 - z2)

infixl 7 `times`
times :: Double -> Vector -> Vector
times t (Vector x y z) = Vector (t*x) (t*y) (t*z)

infixl 7 `dot`
dot :: Vector -> Vector -> Double
dot (Vector x1 y1 z1) (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

sqVecLen :: Vector -> Double
sqVecLen v = v `dot` v

vecLen :: Vector -> Double
vecLen = sqrt . sqVecLen

normalize :: Vector -> Vector
normalize v = (1 / vecLen v) `times` v

data Color =
    Greyscale Double |
    RGB Double Double Double
    deriving (Eq, Show)

toWordList :: Color -> [Word8]
toWordList = map (round . (* 255)) . toDoubleList where
    toDoubleList (Greyscale c) = [c, c, c, 1]
    toDoubleList (RGB r g b) = [r, g, b, 1]

infixl 7 `cMult`
cMult :: Double -> Color -> Color
cMult t (Greyscale c) = Greyscale (t * c)
cMult t (RGB r g b) = RGB (t * r) (t * g) (t * b)

black :: Color
black = RGB 0 0 0
