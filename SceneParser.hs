module SceneParser (parse) where

import Scene
import Geometry
import DataTypes

import Text.Parsec.Token

parse :: String -> Either String (Scene RGB)
parse = undefined
