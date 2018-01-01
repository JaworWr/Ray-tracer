module Geometry where

import DataTypes

eps :: Double
eps = 0.00000001

-- Typ danych reprezentujący promień dany w postaci x + t*d
-- gdzie x to źródło promienia a d to wektor normalny wskazujący jego kierunek
data Ray = Ray { origin :: Vector, dir :: Vector } deriving (Show, Eq)

-- funkcja tworząca promienie
makeRay :: Vector -> Vector -> Ray
makeRay o d = Ray o (normalize d)

-- funkcja do obliczania punktów leżących na promieniu
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
    Directional Vector
    deriving (Eq, Show)

makeDirectional :: Vector -> LightSource
makeDirectional = Directional . normalize . times (-1)

makeShadowRay :: LightSource -> Vector -> Ray
makeShadowRay (Directional i) x = makeRay x i

getLight :: LightSource -> Vector -> Double
getLight (Directional i) d = max 0 $ i `dot` d
