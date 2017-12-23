module Geometry where

import DataTypes

data Ray = Ray { origin :: Vector, dir :: Vector } deriving (Show, Eq)

makeRay :: Vector -> Vector -> Ray
makeRay o d = Ray o (normalize d)

getRayPoint :: Ray -> Double -> Vector
getRayPoint (Ray o d) t = o +. t `times` d

reflectRay :: Vector -> Vector -> Ray -> Ray
reflectRay x n (Ray _ d) = makeRay x $ d -. 2 * (n `dot` d) `times` n

data Geometry =
    Sphere Vector Double |
    Plane Vector Vector
    deriving (Eq, Show)

makeSphere :: Vector -> Double -> Geometry
makeSphere = Sphere

makePlane :: Vector -> Vector -> Geometry
makePlane o d = Plane o (normalize d)

normalVector :: Geometry -> Vector -> Vector
normalVector (Sphere c _) x = normalize (x -. c)
normalVector (Plane _ n) _ = n

intersect :: Geometry -> Ray -> [Double]
intersect (Sphere c r) (Ray o d)
    | delta < 0 = []
    | otherwise = [-doc - sqrt delta, -doc + sqrt delta]
    where
        doc = d `dot` (o -. c)
        delta = doc * doc - sqVecLen (o -. c) + r * r
intersect (Plane po pd) (Ray o d) =
    [((po -. o) `dot` pd) / (d `dot` pd)]

reflect :: Geometry -> Vector -> Ray -> Ray
reflect g x = reflectRay x (normalVector g x)
