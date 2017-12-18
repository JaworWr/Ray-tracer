module Geometry where

import DataTypes

data Ray = Ray { origin :: Vector, dir :: Vector } deriving (Show, Eq)

makeRay :: Vector -> Vector -> Ray
makeRay o d = Ray o (normalize d)

data Geometry =
    Sphere Vector Double |
    Plane Vector Vector
    deriving (Eq, Show)

makeSphere :: Vector -> Double -> Geometry
makeSphere = Sphere

makePlane :: Vector -> Vector -> Geometry
makePlane o d = Plane o (normalize d)

intersect :: Ray -> Geometry -> [Double]
intersect (Ray o d) (Sphere c r)
    | delta < 0 = []
    | otherwise = [-doc - sqrt delta, -doc + sqrt delta]
    where
        doc = d `dot` (o `sub` c)
        delta = doc^2 - sqVecLen (o `sub` c) + r^2
intersect (Ray o d) (Plane po pd) =
    [((po `sub` o) `dot` pd) / d `dot` pd]
