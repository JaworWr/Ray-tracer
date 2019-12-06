{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}

module Scene where

import Geometry

-- typ danych reprezentujący scenę
data Scene t = Scene {
    pxWidth :: Int,
    pxHeight :: Int,
    scrWidth :: Double,
    scrHeight :: Double,
    depth :: Double,
    bgColor :: t,
    rayDepth :: Int,
    lights :: [LightSource t],
    objects :: [Object t]
} deriving (Show)

-- typ danych reprezentujący możliwe rodzaje powierzchni obiektów
data Surface t =
    Diffusive t |
    Reflective |
    Luminous t |
    Mixed [(Double, Surface t)]
    deriving (Eq, Show)

-- typ danych reprezentujący obiekty sceny
data Object t = ∀ g . (Show g, Geometry g) => Object g (Surface t)

-- reprezentacja tekstowa geometrii obiektu
showGeometry :: Object t -> String
showGeometry (Object g _) = show g

-- powierzchnia obiektu
surface :: Object t -> Surface t
surface (Object _ s) = s

instance Show t => Show (Object t) where
    show o = "Object " ++ showGeometry o ++ " " ++ show (surface o)

instance Geometry (Object t) where
    normalVector (Object g _) = normalVector g
    intersect (Object g _) = intersect g
    geometryValid (Object g _) = geometryValid g
