module Scene where

import DataTypes
import Geometry

-- funkcja renderująca scenę
render :: Color t => Scene t -> Image t
render s = Image (pxWidth s) (pxHeight s) $
    map (traceRay (bgColor s) (lights s) (objects s)) (makeRays s)

-- typ danych reprezentujący możliwe rodzaje powierzchni obiektów
data Surface t =
    Diffusive t |
    Reflective |
    Lit |
    Mixed [(Surface t, Double)]
    deriving (Eq, Show)

-- typ danych reprezentujący obiekty sceny
data Object t = Object {
    geometry :: Geometry,
    surface :: Surface t
} deriving (Eq, Show)

-- typ danych reprezentujący scenę
data Scene t = Scene {
    objects :: [Object t],
    lights :: [LightSource t],
    pxWidth :: Int,
    pxHeight :: Int,
    scrWidth :: Double,
    scrHeight :: Double,
    bgColor :: t,
    depth :: Double
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
    . filter ((> eps) . fst)
    . concatMap pairWithIntersects
    where
        pairWithIntersects o = map (\x -> (x, o)) $ intersect r $ geometry o
        minIntersect [] = Nothing
        minIntersect xs =
            Just $ foldl1 (\acc x -> if fst x < fst acc then x else acc) xs

-- funkcja śledząca promień w celu obliczenia koloru badanego piksela
traceRay :: Color t => t -> [LightSource t] -> [Object t] -> Ray -> t
traceRay c ls xs r = maybe c calcRGB m where
    m = closestIntersect r xs
    calcRGB (t, o) = let x = getRayPoint r t in
        case surface o of
            Diffusive c -> foldl cAdd black
                (map (\l -> traceShadow l t x (normalVector (geometry o) x) xs
                    (makeShadowRay l x)) ls)
                `cMult` c
            _ -> error "Not yet implemented"

-- funkcja obliczająca ilość światła padającego na obiekt
-- poprzez śledzenie dodatkowego promienia
traceShadow :: Color t => LightSource t -> Double -> Vector -> Vector -> [Object t] -> Ray -> t
traceShadow l d x n xs r = maybe (getLight l d x n) calcLight m where
    m = closestIntersect r xs >>= \x -> if lIntersect l d $ fst x then return x else Nothing
    calcLight (t, o) = let x = getRayPoint r t in
        case surface o of
            Diffusive _ -> black
            _ -> error "Not yet implemented"

-- funkcja tworząca listę promieni odpowiadających pikselom tworzonego obrazu
makeRays :: Scene t -> [Ray]
makeRays s = map makePixelRay pixelVectors where
    makePixelRay = makeRay (Vector 0 0 (- depth s))
    diffX = scrWidth s / fromIntegral (pxWidth s)
    diffY = scrHeight s / fromIntegral (pxHeight s)
    shiftX x = fromIntegral (x - (pxHeight s `div` 2)) * diffX
    shiftY y = fromIntegral (y - (pxWidth s `div` 2)) * diffY
    pixelVectors = makePixelVectors 0 0
    makePixelVectors x y =
        Vector (shiftX x) (shiftY y) (depth s) : pixelVectorsNext x y
    pixelVectorsNext x y
        | x < pxWidth s - 1 = makePixelVectors (x+1) y
        | y < pxHeight s -1 = makePixelVectors 0 (y+1)
        | otherwise = []
