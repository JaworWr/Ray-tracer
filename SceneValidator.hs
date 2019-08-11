module SceneValidator (validateScene) where

import DataTypes
import Geometry
import Scene
import Control.Monad.Except
import Data.Char

validateScene :: (Show t, Color t) => Scene t -> Either String ()
validateScene s = do
    validateMinValue "image width" (pxWidth s) 0
    validateMinValue "image height" (pxHeight s) 0
    validateMinValue "canvas width" (scrWidth s) 0
    validateMinValue "canvas height" (scrHeight s) 0
    validateMinValue "depth" (depth s) eps
    validateColor "scene background" (bgColor s)
    validateMinValue "ray depth" (rayDepth s) 0
    mapM_ validateLightSource (enumerate $ lights s)
    mapM_ validateObject (enumerate $ objects s)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [1..]

minValueStr :: Show t => String -> t -> String
minValueStr vt m = capitalize vt ++ " must be greater than or equal to " ++ show m where
    capitalize [] = []
    capitalize (x:xs) = toUpper x : xs

validateMinValue :: (Show t, Ord t) => String -> t -> t -> Either String ()
validateMinValue vt v m = when (v < m) $ throwError
    ("Invalid " ++ vt ++ " value: " ++ show v ++ ". " ++ minValueStr vt m)

validateColor :: (Show t, Color t) => String -> t -> Either String ()
validateColor ot c = unless (colorValid c) $ throwError invalidColorStr where
    invalidColorStr = "Invalid " ++ ot ++ " color: " ++ show c
        ++ ". " ++ minValueStr "all components" 0

validateLightSource :: (Show t, Color t) => (Int, LightSource t) -> Either String ()
validateLightSource (i, s) = validateColor "lightsource" (lightSourceColor s)
    `catchError` throwLSError
    where
        invalidLSErrorStr = "Lightsource #" ++ show i ++ ":\n"
        throwLSError = throwError . (invalidLSErrorStr ++)

validateObject :: (Show t, Color t) => (Int, Object t) -> Either String ()
validateObject (i, o) = do
    unless (geometryValid o) $
        throwObjError ("Invalid geometry: " ++ showGeometry o)
    validateSurface (surface o) `catchError` throwObjError
    where
        invalidObjectStr = "Object #" ++ show i ++ ":\n"
        throwObjError = throwError . (invalidObjectStr ++)

validateSurface :: (Show t, Color t) => Surface t -> Either String ()
validateSurface (Diffusive c) = validateColor "surface" c
validateSurface Reflective = return ()
validateSurface (Luminous c) = validateColor "surface" c
validateSurface (Mixed ss) = mapM_ validateMixedSurfElem $ enumerate ss

validateMixedSurfElem :: (Show t, Color t) => (Int, (Double, Surface t)) -> Either String ()
validateMixedSurfElem (i, (w, s)) = do
    validateMinValue "mixed surface component weight" w 0
    validateSurface s `catchError` throwMixedSurfElemError
    where
        invalidMixedSurfElemStr = "Mixed surface element #" ++ show i ++ ":\n"
        throwMixedSurfElemError = throwError . (invalidMixedSurfElemStr ++)
