module SceneParser where

import Scene
import Geometry
import DataTypes

parse :: IO Scene
parse = return $ Scene
    [Object (makeSphere (Vector 0 0 100) 20) (Diffusive $ RGB 1 0 1),
    Object (makePlane (Vector 0 0 200) (Vector (-1) 0 (-2))) (Diffusive $ RGB 0 1 0)]
    (makeUniform $ Vector 1 0 1)
    200 200 20 20 10
