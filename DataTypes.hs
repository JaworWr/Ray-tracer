module DataTypes where

import Data.Word

data Vector = Vector Double Double Double deriving (Show, Eq)

add :: Vector -> Vector -> Vector
add (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

sub :: Vector -> Vector -> Vector
sub (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 - x2) (y1 - y2) (z1 - z2)

mult :: Double -> Vector -> Vector
mult t (Vector x y z) = Vector (t*x) (t*y) (t*z)

dot :: Vector -> Vector -> Double
dot (Vector x1 y1 z1) (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

sqVecLen :: Vector -> Double
sqVecLen v = dot v v

vecLen :: Vector -> Double
vecLen = sqrt . sqVecLen

normalize :: Vector -> Vector
normalize v = mult (1 / vecLen v) v

data Color =
    Greyscale Word8 |
    RGB Word8 Word8 Word8
    deriving (Eq, Show)

toWordList :: Color -> [Word8]
toWordList (Greyscale c) = [c, c, c, 0xff]
toWordList (RGB r g b) = [r, g, b, 0xff]
