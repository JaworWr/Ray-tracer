module SceneParser (parseScene) where

import Scene
import Geometry
import DataTypes

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.String

-- główna funkcja parsująca scenę
parseScene :: String -> String -> Either String (Scene RGB)
parseScene name s = case parse pScene name s of
    Left e -> Left $ show e
    Right s -> return s

-- definicja językka opisującego scenę
def = emptyDef {
    commentLine = "#",
    caseSensitive = False,
    opStart = oneOf "+-",
    opLetter = oneOf "+-",
    reservedNames = ["imwidth", "imheight", "scrwidth",
                    "scrheight", "depth", "bgcolor",
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
pInt = fromInteger <$> integer tokenParser <?> "Integer"

-- parser liczb typu Double, parsuje również liczby całkowite
pDouble :: Parser Double
pDouble = do
    sign <- getSign
    n <- naturalOrFloat tokenParser
    case n of
        Left x -> return $ sign * fromInteger x
        Right x -> return $ sign * x
    <?> "Double"
    where
        pOp = reservedOp tokenParser
        getSign = (pOp "-" >> return (-1)) <|> (optional (pOp "+") >> return 1)

-- parsuje liczbę a następnie zamienia liczby ujemne na 0
pPositive :: (Num t, Ord t) => Parser t -> Parser t
pPositive p = max 0 <$> p

-- parser wektorów
pVector :: Parser Vector
pVector = Vector <$> pDouble <*> pDouble <*> pDouble

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
    makeRGB <$> pPositive pDouble <*> pPositive pDouble <*> pPositive pDouble
    ] <?> "color"

-- parser źródeł światła
pLights :: Parser [LightSource RGB]
pLights = pKw "lights" >> many pLight

-- parser pojedynczego źródła światła
pLight :: Parser (LightSource RGB)
pLight = choice [
    pKw "directional" >> makeDirectional <$> pPositive pDouble <*> pRGB <*> pVector,
    pKw "spherical" >> makeSpherical <$> pPositive pDouble <*> pRGB <*> pVector
    ]

-- parser obiektów
pObjects :: Parser [Object RGB]
pObjects = pKw "objects" >> many pObject

-- parser pojedynczego obiektu
pObject :: Parser (Object RGB)
pObject = Object <$> pGeometry <*> pSurface

-- parser obiektu w przestrzeni trójwymiarowej
pGeometry :: Parser Geometry
pGeometry = choice [
    pKw "sphere" >> makeSphere <$> pVector <*> pPositive pDouble,
    pKw "plane" >> makePlane <$> pVector <*> pVector
    ]

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
pScene = Scene <$>
    (pKw "imwidth" >> pPositive pInt) <*>
    (pKw "imheight" >> pPositive pInt) <*>
    (pKw "scrwidth" >> pPositive pDouble) <*>
    (pKw "scrheight" >> pPositive pDouble) <*>
    (pKw "depth" >> pPositive pDouble) <*>
    option black (pKw "bgcolor" >> pRGB) <*>
    pLights <*>
    pObjects
