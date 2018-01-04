module SceneParser (parse) where

import Scene
import Geometry
import DataTypes

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.String

parseScene :: String -> String -> Either String (Scene RGB)
parseScene name s = case parse pScene name s of
    Left e -> Left $ show e
    Right s -> return s

def = emptyDef {
    commentLine = "#",
    caseSensitive = False,
    reservedNames = ["imwidth", "imheight", "scrwidth",
                    "scrheight", "depth", "bgcolor",
                    "lights", "directional", "spherical",
                    "objects", "sphere", "plane",
                    "diffusive", "reflective", "luminous"]
}

tokenParser = makeTokenParser def

pKw = reserved tokenParser

pInt :: Parser Int
pInt = fromInteger <$> integer tokenParser

pDouble :: Parser Double
pDouble = float tokenParser

pPositive :: (Num t, Ord t) => Parser t -> Parser t
pPositive p = max 0 <$> p

pVector :: Parser Vector
pVector = Vector <$> pDouble <*> pDouble <*> pDouble

pRGB :: Parser RGB
pRGB = makeRGB <$> pPositive pDouble <*> pPositive pDouble <*> pPositive pDouble

pLights :: Parser [LightSource RGB]
pLights = pKw "lights" >> many pLight

pLight :: Parser (LightSource RGB)
pLight = choice [
    pKw "directional" >> makeDirectional <$> pPositive pDouble <*> pRGB <*> pVector,
    pKw "spherical" >> makeDirectional <$> pPositive pDouble <*> pRGB <*> pVector
    ]

pScene :: Parser (Scene RGB)
pScene = Scene <$>
    undefined <*>
    pLights <*>
    (pKw "imwidth" >> pPositive pInt) <*>
    (pKw "imheight" >> pPositive pInt) <*>
    (pKw "scrwidth" >> pPositive pDouble) <*>
    (pKw "scrheight" >> pPositive pDouble) <*>
    (pKw "depth" >> pPositive pDouble) <*>
    option black (pKw "bgcolor" >> pRGB)
