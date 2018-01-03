module Geometry where

import DataTypes

eps :: Double
eps = 0.00000001

inf :: Double
inf = 1/0

-- Typ danych reprezentujący promień dany w postaci x + t*d
-- gdzie x to źródło promienia a d to wektor normalny wskazujący jego kierunek
data Ray = Ray { origin :: Vector, dir :: Vector } deriving (Show, Eq)

-- konstruktor promieni
makeRay :: Vector -> Vector -> Ray
makeRay o d = Ray o (normalize d)

-- funkcja do obliczania punktów leżących na promieniu
getRayPoint :: Ray -> Double -> Vector
getRayPoint (Ray o d) t = o +. t `times` d

-- funkcja obliczająca promień odbity do danego promienia w danym punkcie x
-- względem wektora normalnego n
reflectRay :: Vector -> Vector -> Ray -> Ray
reflectRay x n (Ray _ d) = makeRay x $ d -. 2 * (n `dot` d) `times` n

-- typ danych reprezentujący obiekty w przestrzeni trójwymiarowej
data Geometry =
    Sphere Vector Double |
    Plane Vector Vector
    deriving (Eq, Show)

-- konstruktor sfery
makeSphere :: Vector -> Double -> Geometry
makeSphere = Sphere

-- konstruktor płaszczyzny, przyjmujący jej początek
-- oraz wektor prostopadły (niekoniecznie normalny)
makePlane :: Vector -> Vector -> Geometry
makePlane o d = Plane o (normalize d)

-- funkcja wyznaczająca wektor normalny do dango obiektu w danym punkcie
normalVector :: Geometry -> Vector -> Vector
normalVector (Sphere c _) x = normalize (x -. c)
normalVector (Plane _ n) _ = n

-- funkcja wyznaczająca listę wartości t
-- dla których promień x + t*d przecina dany obiekt
intersect :: Ray -> Geometry -> [Double]
intersect (Ray o d) (Sphere c r)
    | delta < 0 = []
    | otherwise = [-doc - sqrt delta, -doc + sqrt delta]
    where
        doc = d `dot` (o -. c)
        delta = doc * doc - sqVecLen (o -. c) + r * r
intersect (Ray o d) (Plane po pd) =
    [((po -. o) `dot` pd) / (d `dot` pd)]

-- funkcja wyznaczająca promień odbity w danym punkcie obiektu
reflect :: Geometry -> Vector -> Ray -> Ray
reflect g x = reflectRay x (normalVector g x)

-- typ danych reprezentujący źródło światła
data LightSource t =
    Directional t Vector |
    Spherical t Vector
    deriving (Eq, Show)

-- konstruktor kierunkowego źródła światła
makeDirectional :: t -> Vector -> LightSource t
makeDirectional c = Directional c . normalize . times (-1)

-- konstruktor punktowego źródła światła
makeSpherical :: t -> Vector -> LightSource t
makeSpherical = Spherical

-- konstruktor promienia wyznaczającego oświetlenie obiektu
makeShadowRay :: LightSource t -> Vector -> Ray
makeShadowRay (Directional _ i) x = makeRay x i
makeShadowRay (Spherical _ s) x = makeRay x (s -. x)

-- ilość światła padającego na obiekt
getLight :: Color t => LightSource t -> Vector -> Vector -> t
getLight (Directional c i) _ n = max 0 (i `dot` n) `cTimes` c
getLight (Spherical c s) x n = let d = s -. x in
    max 0 (normalize d `dot` n) / sqVecLen d `cTimes` c

lIntersect :: LightSource t -> Double -> Vector -> Bool
lIntersect (Directional _ _) _ _ = True
lIntersect (Spherical _ s) d x = d * d < sqVecLen (x -. s)
