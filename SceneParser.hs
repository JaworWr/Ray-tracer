module SceneParser (parseScene) where

import Scene
import Geometry
import DataTypes

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Perm
import Control.Monad.Except

-- główna funkcja parsująca scenę
parseScene :: String -> String -> Either ParseError (Scene RGB)
parseScene = parse pMain

-- definicja językka opisującego scenę
def = emptyDef {
    commentLine = "#",
    caseSensitive = False,
    opStart = oneOf "+-",
    opLetter = oneOf "+-",
    reservedNames = ["imwidth", "imheight", "canvwidth",
                    "canvheight", "depth", "bgcolor", "raydepth",
                    "lights", "directional", "spherical",
                    "objects", "sphere", "plane",
                    "diffusive", "reflective", "luminous", "mixed",
                    "black", "white", "red", "green", "blue",
                    "cyan", "magenta", "yellow"]
}

tokenParser = makeTokenParser def

pKw = reserved tokenParser

-- parser liczb całkowitych
pInt :: Parser Int
pInt = fromInteger <$> integer tokenParser <?> "integer"

-- parser liczb typu Double, parsuje również liczby całkowite
pDouble :: Parser Double
pDouble = do
    sign <- getSign
    n <- naturalOrFloat tokenParser
    case n of
        Left x -> return $ sign * fromInteger x
        Right x -> return $ sign * x
    <?> "double"
    where
        pOp = reservedOp tokenParser
        getSign = (pOp "-" >> return (-1)) <|> (optional (pOp "+") >> return 1)

-- parser wektorów
pVector :: Parser Vector
pVector = Vector <$> pDouble <*> pDouble <*> pDouble <?> "vector"

-- parser kolorów w postaci RGB
pRGB :: Parser RGB
pRGB = choice [
    pKw "black" >> return black,
    pKw "white" >> return white,
    pKw "red" >> return red,
    pKw "green" >> return green,
    pKw "blue" >> return blue,
    pKw "cyan" >> return cyan,
    pKw "magenta" >> return magenta,
    pKw "yellow" >> return yellow,
    makeRGB <$> pDouble <*> pDouble <*> pDouble
    ] <?> "color"

-- parser źródeł światła
pLights :: Parser [LightSource RGB]
pLights = pKw "lights" >> many pLight

-- parser pojedynczego źródła światła
pLight :: Parser (LightSource RGB)
pLight = choice [
    pKw "directional" >> makeDirectional <$> pDouble <*> pRGB <*> pVector,
    pKw "spherical" >> makeSpherical <$> pDouble <*> pRGB <*> pVector
    ]

-- parser obiektów
pObjects :: Parser [Object RGB]
pObjects = pKw "objects" >> many pObject

-- parser pojedynczego obiektu
pObject :: Parser (Object RGB)
pObject = choice [
    toObjectParser pSphere,
    toObjectParser pPlane
    ]
    where
        toObjectParser p = Object <$> p <*> pSurface

-- parser sfery
pSphere :: Parser Sphere
pSphere = pKw "sphere" >> makeSphere <$> pVector <*> pDouble

-- parser płaszczyzny
pPlane :: Parser Plane
pPlane = pKw "plane" >> makePlane <$> pVector <*> pVector

-- parser powierzchni obiektów
pSurface :: Parser (Surface RGB)
pSurface = choice [
    pKw "diffusive" >> Diffusive <$> pRGB,
    pKw "reflective" >> return Reflective,
    pKw "luminous" >> Luminous <$> pRGB,
    pKw "mixed" >> Mixed <$> many1 ((,) <$> pDouble <*> pSurface)
    ]

-- parser sceny
pScene :: Parser (Scene RGB)
pScene = permute (Scene <$$>
    (pKw "imwidth" >> pInt) <||>
    (pKw "imheight" >> pInt) <||>
    (pKw "canvwidth" >> pDouble) <||>
    (pKw "canvheight" >> pDouble) <||>
    (pKw "depth" >> pDouble) <|?>
    (black, pKw "bgcolor" >> pRGB) <|?>
    (4, pKw "rayDepth" >> pInt)) <*>
    pLights <*>
    pObjects

pMain :: Parser (Scene RGB)
pMain = whiteSpace tokenParser >> pScene <* eof
