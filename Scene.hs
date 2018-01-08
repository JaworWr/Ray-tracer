module Scene where

import DataTypes
import Geometry
import Data.Maybe

-- funkcja renderująca scenę
render :: Color t => Scene t -> Image t
render s = Image (pxWidth s) (pxHeight s) $
    map (traceRay 5 (bgColor s) (lights s) (objects s)) (makeRays s)

-- typ danych reprezentujący możliwe rodzaje powierzchni obiektów
data Surface t =
    Diffusive t |
    Reflective |
    Luminous t |
    Mixed [(Double, Surface t)]
    deriving (Eq, Show)

-- typ danych reprezentujący obiekty sceny
data Object t = Object {
    geometry :: Geometry,
    surface :: Surface t
} deriving (Eq, Show)

-- typ danych reprezentujący scenę
data Scene t = Scene {
    pxWidth :: Int,
    pxHeight :: Int,
    scrWidth :: Double,
    scrHeight :: Double,
    depth :: Double,
    bgColor :: t,
    lights :: [LightSource t],
    objects :: [Object t]
} deriving (Eq, Show)

-- typ danych reprezentujący obraz w postaci listy pikseli
data Image t = Image {
    imWidth :: Int,
    imHeight :: Int,
    imPixels :: [t]
} deriving (Eq, Show)

-- funkcja znajdująca, o ile to możliwe, najmniejszą dodatnią wartość t,
-- dla której promień x + t*d przecina przecina pewien obiekt sceny, a także ów obiekt
closestIntersect :: Ray -> [Object t] -> Maybe (Double, Object t)
closestIntersect r = minIntersect
    . filter ((> 0) . fst)
    . concatMap pairWithIntersects
    where
        pairWithIntersects o = map (\x -> (x, o)) $ intersect r $ geometry o
        minIntersect [] = Nothing
        minIntersect xs =
            Just $ foldl1 (\acc x -> if fst x < fst acc then x else acc) xs

-- funkcja śledząca promień w celu obliczenia koloru badanego piksela
traceRay :: Color t => Int -> t -> [LightSource t] -> [Object t] -> Ray -> t
traceRay 0 bg _ _ _ = bg
traceRay d bg ls xs r = maybe bg calcRGB m where
    m = closestIntersect r xs
    calcRGB (t, o) = surfaceColor t o (getRayPoint r t) (surface o)
    surfaceColor t o x (Diffusive c) = let n = normalVector (geometry o) x in
        foldl cAdd black
            (map (\l -> traceShadow l x n xs (makeShadowRay n l x)) ls)
        `cMult` c
    surfaceColor t o x Reflective =
        traceRay (d-1) bg ls xs (reflect (geometry o) x r)
    surfaceColor t o x (Luminous c) =
        normalVector (geometry o) x `dot` ((-1) `times` dir r) `cTimes` c
    surfaceColor t o x (Mixed xs) =
        foldl (\acc (v, s) -> acc `cAdd` v `cTimes` surfaceColor t o x s) black xs

-- funkcja sprawdzająca czy wybrany obiekt otrzymuje światło z badanego źródła
traceShadow :: Color t => LightSource t -> Vector -> Vector -> [Object t] -> Ray -> t
traceShadow l x n xs r = if isJust m then black else getLight l x n where
    m = closestIntersect r xs >>=
        \p -> if lIntersect l (fst p) x then return p else Nothing

-- funkcja tworząca listę promieni odpowiadających pikselom tworzonego obrazu
makeRays :: Scene t -> [Ray]
makeRays s = makePixelRays 0 0 where
    l = -0.5 * scrWidth s
    b = -0.5 * scrHeight s
    pxX n = l + scrWidth s * (fromIntegral n + 0.5) / fromIntegral (pxWidth s)
    pxY n = b + scrHeight s * (fromIntegral n + 0.5) / fromIntegral (pxHeight s)
    makePixelRays x y = makeRay (Vector 0 0 (- depth s))
        (Vector (pxX x) (pxY y) (depth s)) : pixelRaysNext x y
    pixelRaysNext x y
        | x < pxWidth s - 1 = makePixelRays (x+1) y
        | y < pxHeight s - 1 = makePixelRays 0 (y+1)
        | otherwise = []
