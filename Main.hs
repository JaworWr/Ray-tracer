module Main where

import DataTypes

main :: IO ()
main = print . vecLen $ Vector 1 1 1
