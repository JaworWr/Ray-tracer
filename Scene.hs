module Scene where

import DataTypes
import Geometry

-- funkcja renderująca scenę
render :: Scene -> Image
render s = Image (pxWidth s) (pxHeight s) $
    map (traceRay (bgColor s) (light s) (objects s)) (makeRays s)

-- typ danych reprezentujący możliwe rodzaje powierzchni obiektów
data Surface =
    Diffusive Color |
    Reflective |
    Lit |
    Mixed [(Surface, Double)]
    deriving (Eq, Show)

-- typ danych reprezentujący obiekty sceny
data Object = Object {
    geometry :: Geometry,
    surface :: Surface
} deriving (Eq, Show)

-- typ danych reprezentujący scenę
data Scene = Scene {
    objects :: [Object],
    light :: LightSource,
    pxWidth :: Int,
    pxHeight :: Int,
    scrWidth :: Double,
    scrHeight :: Double,
    bgColor :: Color,
    depth :: Double
} deriving (Eq, Show)

-- typ danych reprezentujący obraz w postaci listy pikseli
data Image = Image {
    imWidth :: Int,
    imHeight :: Int,
    imPixels :: [Color]
} deriving (Eq, Show)

-- funkcja znajdująca, o ile to możliwe, najmniejszą dodatnią wartość t,
-- dla której promień x + t*d przecina przecina pewien obiekt sceny, a także ów obiekt
closestIntersect :: Ray -> [Object] -> Maybe (Double, Object)
closestIntersect r = minIntersect
    . filter ((> eps) . fst)
    . concatMap pairWithIntersects
    where
        pairWithIntersects o = map (\x -> (x, o)) $ intersect r $ geometry o
        minIntersect [] = Nothing
        minIntersect xs =
            Just $ foldl1 (\acc x -> if fst x < fst acc then x else acc) xs

-- funkcja śledząca promień w celu obliczenia koloru badanego piksela
traceRay :: Color -> LightSource -> [Object] -> Ray -> Color
traceRay c l xs r = maybe c calcColor m where
    m = closestIntersect r xs
    calcColor (t, o) = let x = getRayPoint r t in
        case surface o of
            Diffusive c ->
                traceShadow l (normalVector (geometry o) x) xs
                    (makeShadowRay l x) `cMult` c
            _ -> error "Not yet implemented"

-- funkcja obliczająca ilość światła padającego na obiekt
-- poprzez śledzenie dodatkowego promienia
traceShadow :: LightSource -> Vector -> [Object] -> Ray -> Double
traceShadow l n xs r = maybe (getLight l n) calcLight m where
    m = closestIntersect r xs
    calcLight (t, o) = let x = getRayPoint r t in
        case surface o of
            Diffusive _ -> 0
            _ -> error "Not yet implemented"

-- funkcja tworząca listę promieni odpowiadających pikselom tworzonego obrazu
makeRays :: Scene -> [Ray]
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
