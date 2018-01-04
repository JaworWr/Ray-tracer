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

pScene :: Parser (Scene RGB)
pScene = Scene <$>
    undefined <*>
    undefined <*>
    (pKw "imwidth" >> pPositive pInt) <*>
    (pKw "imheight" >> pPositive pInt) <*>
    (pKw "width" >> pPositive pDouble) <*>
    (pKw "height" >> pPositive pDouble) <*>
    (pKw "depth" >> pPositive pDouble) <*>
    option black (pKw "bgcolor" >> pRGB)
