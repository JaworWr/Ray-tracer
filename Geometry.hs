module Geometry where

import DataTypes

eps :: Double
eps = 0.00000001

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

intersect :: Ray -> Geometry -> [Double]
intersect (Ray o d) (Sphere c r)
    | delta < 0 = []
    | otherwise = [-doc - sqrt delta, -doc + sqrt delta]
    where
        doc = d `dot` (o -. c)
        delta = doc * doc - sqVecLen (o -. c) + r * r
intersect (Ray o d) (Plane po pd) =
    [((po -. o) `dot` pd) / (d `dot` pd)]

reflect :: Geometry -> Vector -> Ray -> Ray
reflect g x = reflectRay x (normalVector g x)

data LightSource =
    Uniform Vector
    deriving (Eq, Show)

makeUniform :: Vector -> LightSource
makeUniform = Uniform . normalize . times (-1)

getLight :: LightSource -> Ray -> Double
getLight (Uniform i) (Ray _ d) = max 0 $ i `dot` d -- let c = max 0 $ i `dot` d in trace (show c ++ ", " ++ show (i `dot` d)) c
