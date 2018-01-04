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
    Luminous t |
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
                (map (\l -> traceShadow l x (normalVector (geometry o) x) xs
                    (makeShadowRay l x)) ls)
                `cMult` c
            Luminous c ->
                normalVector (geometry o) x `dot` ((-1) `times` dir r) `cTimes` c
            _ -> error "Not yet implemented"

-- funkcja obliczająca ilość światła padającego na obiekt
-- poprzez śledzenie dodatkowego promienia
traceShadow :: Color t => LightSource t -> Vector -> Vector -> [Object t] -> Ray -> t
traceShadow l x n xs r = maybe (getLight l x n) calcLight m where
    m = closestIntersect r xs >>=
        \p -> if lIntersect l (fst p) x then return p else Nothing
    calcLight (t, o) = let x = getRayPoint r t in
        case surface o of
            Diffusive _ -> black
            Luminous _ -> black
            _ -> error "Not yet implemented"

-- funkcja tworząca listę promieni odpowiadających pikselom tworzonego obrazu
makeRays :: Scene t -> [Ray]
makeRays s = pixelRays where
    diffX = scrWidth s / fromIntegral (pxWidth s)
    diffY = scrHeight s / fromIntegral (pxHeight s)
    shiftX x = fromIntegral (x - (pxHeight s `div` 2)) * diffX
    shiftY y = fromIntegral (y - (pxWidth s `div` 2)) * diffY
    pixelRays = makePixelRay 0 0
    makePixelRay x y = makeRay (Vector (shiftX x) (shiftY y) 0)
        (Vector (shiftX x) (shiftY y) (depth s)) : pixelRaysNext x y
    pixelRaysNext x y
        | x < pxWidth s - 1 = makePixelRay (x+1) y
        | y < pxHeight s -1 = makePixelRay 0 (y+1)
        | otherwise = []
