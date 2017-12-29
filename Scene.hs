module Scene where

import DataTypes
import Geometry

data Surface =
    Diffusive Color |
    Reflective
    deriving (Eq, Show)

data Object = Object {
    geometry :: Geometry,
    surface :: Surface
} deriving (Eq, Show)

closestIntersect :: Ray -> [Object] -> Maybe (Double, Object)
closestIntersect r = minIntersect
    . filter ((> 0) . fst)
    . concatMap pairWithIntersects
    where
        pairWithIntersects o = map (\x -> (x, o)) $ intersect r $ geometry o
        minIntersect [] = Nothing
        minIntersect xs =
            Just $ foldl1 (\acc x -> if fst x < fst acc then x else acc) xs

traceRay :: LightSource -> [Object] -> Ray -> Color
traceRay l xs r = maybe black calcColor m where
    m = closestIntersect r xs
    calcColor (t, o) = let x = getRayPoint r t in
        case surface o of
            Diffusive c ->
                traceShadow l xs (reflect (geometry o) x r) `cMult` c
            _ -> error "Not yet implemented"

traceShadow :: LightSource -> [Object] -> Ray -> Double
traceShadow l xs r =  maybe (getLight l r) calcLight m where
    m = closestIntersect r xs
    calcLight (t, o) = let x = getRayPoint r t in
        case surface o of
            Diffusive c -> 0
            _ -> error "Not yet implemented"

makeRays :: Scene -> [Ray]
makeRays s = map makePixelRay pixelVectors where
    makePixelRay = makeRay (Vector 0 0 0)
    diffX = scrWidth s / fromIntegral (pxWidth s)
    diffY = scrHeight s / fromIntegral (pxHeight s)
    shiftX x = fromIntegral (x - (pxHeight s `div` 2)) * diffX
    shiftY y = fromIntegral (y - (pxWidth s `div` 2)) * diffY
    pixelVectors = makePixelVectors 0 0
    makePixelVectors x y =
        Vector (shiftX x) (shiftY y) (depth s) : pixelVectorsNext x y
    pixelVectorsNext x y
        | x < pxWidth s = makePixelVectors (x+1) y
        | y < pxHeight s = makePixelVectors 0 (y+1)
        | otherwise = []

render :: Scene -> Image
render s = Image (pxWidth s) (pxHeight s) $
    map (traceRay (light s) (objects s)) (makeRays s)

data Scene = Scene {
    objects :: [Object],
    light :: LightSource,
    pxWidth :: Int,
    pxHeight :: Int,
    scrWidth :: Double,
    scrHeight :: Double,
    depth :: Double
} deriving (Eq, Show)

data Image = Image {
    imWidth :: Int,
    imHeight :: Int,
    imPixels :: [Color]
} deriving (Eq, Show)
