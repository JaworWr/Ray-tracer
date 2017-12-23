module Scene where

import DataTypes
import Geometry

data Surface =
    Diffusive Color |
    Reflective
    deriving (Eq, Show)

data Object = Object Geometry Surface deriving (Eq, Show)
