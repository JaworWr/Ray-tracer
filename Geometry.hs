module Geometry where

import DataTypes

eps :: Double
eps = 1e-5

-- typ danych reprezentujący promień dany w postaci x + t*d
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
reflectRay x n (Ray _ d) = makeRay (x +. eps `times` n) $ d -. 2 * (n `dot` d) `times` n

-- klasa obiektów w przestrzeni trójwymiarowej
class Geometry g where
    -- funkcja wyznaczająca wektor normalny do dango obiektu w danym punkcie
    normalVector :: g -> Vector -> Vector
    -- funkcja wyznaczająca listę wartości t
    -- dla których promień x + t*d przecina dany obiekt
    intersect :: g -> Ray -> [Double]

-- konkretne rodzaje obiektów
-- sfera
data Sphere = Sphere Vector Double deriving (Eq, Show)

-- konstruktor sfery
makeSphere :: Vector -> Double -> Sphere
makeSphere = Sphere

instance Geometry Sphere where
    normalVector (Sphere c _) x = normalize (x -. c)
    intersect (Sphere c r) (Ray o d)
        | delta < 0 = []
        | otherwise = [-doc - sqrt delta, -doc + sqrt delta]
        where
            doc = d `dot` (o -. c)
            delta = doc * doc - sqVecLen (o -. c) + r * r

-- płaszczyzna
data Plane = Plane Vector Vector deriving (Eq, Show)

-- konstruktor płaszczyzny, przyjmujący jej początek
-- oraz wektor prostopadły (niekoniecznie normalny)
makePlane :: Vector -> Vector -> Plane
makePlane o d = Plane o (normalize d)

instance Geometry Plane where
    normalVector (Plane _ n) _ = n
    intersect (Plane po pd) (Ray o d) =
        [((po -. o) `dot` pd) / (d `dot` pd)]

-- funkcja wyznaczająca promień odbity w danym punkcie obiektu
reflect :: Geometry g => g -> Vector -> Ray -> Ray
reflect g x = reflectRay x (normalVector g x)

-- typ danych reprezentujący źródło światła
data LightSource t =
    Directional t Vector |
    Spherical t Vector
    deriving (Eq, Show)

-- konstruktor kierunkowego źródła światła
makeDirectional :: Color t => Double -> t -> Vector -> LightSource t
makeDirectional i c = Directional (i `cTimes` c) . normalize . times (-1)

-- konstruktor punktowego źródła światła
makeSpherical :: Color t => Double -> t -> Vector -> LightSource t
makeSpherical i c = Spherical (i `cTimes` c)

-- konstruktor promienia wyznaczającego oświetlenie obiektu
makeShadowRay :: Vector -> LightSource t -> Vector -> Ray
makeShadowRay n (Directional _ i) x = makeRay (x +. eps `times` n) i
makeShadowRay n (Spherical _ s) x = makeRay (x +. eps `times` n) (s -. x)

-- ilość światła padającego na obiekt
getLight :: Color t => LightSource t -> Vector -> Vector -> t
getLight (Directional c i) _ n = max 0 (i `dot` n) `cTimes` c
getLight (Spherical c s) x n = let d = s -. x in
    max 0 (normalize d `dot` n) / sqVecLen d `cTimes` c

-- funkcja sprawdzająca, czy badany obiekt rzeczywiście blokuje rozważane źródło światła
-- (tzn. czy znajduje się między źródłem światła a badaną powierzchnią)
lIntersect :: LightSource t -> Double -> Vector -> Bool
lIntersect (Directional _ _) _ _ = True
lIntersect (Spherical _ s) d x = d * d < sqVecLen (x -. s)
