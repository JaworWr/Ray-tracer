module SceneParser (parse) where

import Scene
import Geometry
import DataTypes

import Data.Char
import Data.List

parse :: String -> Either String (Scene RGB)
parse = parseScene . splitInput . formatInput

type Parser s a = s -> Either String a

missingString :: String -> String
missingString s = "Missing arguments for \'" ++ s ++ "\'"

formatInput :: String -> [[String]]
formatInput =
    filter (not . null) .
    map words .
    filter (\s -> not (null s) && head s /= '#') .
    lines . map toLower

splitInput :: [[String]] -> ([[String]], [[String]], [[String]])
splitInput [] = ([], [], [])
splitInput (("lights":_):xs) = ([], l, o) where
    (l, o) = splitInputAux xs
    splitInputAux [] = ([], [])
    splitInputAux (("objects":_):xs) = ([], xs)
    splitInputAux (x:xs) = let (l, o) = splitInputAux xs in (x:l, o)
splitInput (("objects":_):xs) = ([], [], xs)
splitInput (x:xs) = let (h, l, o) = splitInput xs in (x:h, l, o)

parseDouble :: Parser String Double
parseDouble s = case reads s of
    [(x, [])] -> return x
    _ -> Left "Expected a Double"

parseInt :: Parser String Int
parseInt s = case reads s of
    [(x, [])] -> return x
    _ -> Left "Expected an Int"

parsePositive :: (Num t, Ord t) => Parser String t -> Parser String t
parsePositive p xs = max 0 <$> p xs

parseList :: Parser [String] a -> Parser [[String]] [a]
parseList p = foldl (\acc l -> (:) <$> p l <*> acc) (return [])

parseSurface :: Parser [String] (Surface RGB)
parseSurface ("diffusive":r:g:b:_) = Diffusive <$>
    (makeRGB <$>
        parsePositive parseDouble r <*>
        parsePositive parseDouble g <*>
        parsePositive parseDouble b)
parseSurface ("diffusive":_) = Left $ missingString "diffusive"
parseSurface ("luminous":r:g:b:_) = Luminous <$>
    (makeRGB <$>
        parsePositive parseDouble r <*>
        parsePositive parseDouble g <*>
        parsePositive parseDouble b)
parseSurface ("luminous":_) = Left $ missingString "luminous"
parseSurface ("reflective":_) = return Reflective
parseSurface (s:_) = Left $ "Unknown surface type: \'" ++ s ++ "\'"
parseSurface [] = Left "Missing surface type"

parseObject :: Parser [String] (Object RGB)
parseObject ("sphere":x:y:z:r:xs) = Object <$>
    (makeSphere <$>
        (Vector <$> parseDouble x <*> parseDouble y <*> parseDouble z) <*>
        parseDouble r
    ) <*> parseSurface xs
parseObject ("sphere":_) = Left $ missingString "sphere"
parseObject ("plane":x:y:z:nx:ny:nz:xs) = Object <$>
    (makePlane <$>
        (Vector <$> parseDouble x <*> parseDouble y <*> parseDouble z) <*>
        (Vector <$> parseDouble nx <*> parseDouble ny <*> parseDouble nz)
    ) <*> parseSurface xs
parseObject ("plane":_) = Left $ missingString "plane"
parseObject (o:_) = Left $ "Unknown object: \'" ++ o ++ "\'"

parseObjects :: Parser [[String]] [Object RGB]
parseObjects = parseList parseObject

parseLight :: Parser [String] (LightSource RGB)
parseLight ("directional":r:g:b:i:x:y:z:_) = makeDirectional <$>
    (cTimes <$> parsePositive parseDouble i <*>
        (makeRGB <$>
            parsePositive parseDouble r <*>
            parsePositive parseDouble g <*>
            parsePositive parseDouble b)) <*>
    (Vector <$> parseDouble x <*> parseDouble y <*> parseDouble z)
parseLight ("directional":_) = Left $ missingString "directional"
parseLight ("spherical":r:g:b:i:x:y:z:_) = makeSpherical <$>
    (cTimes <$> parsePositive parseDouble i <*>
    (makeRGB <$>
        parsePositive parseDouble r <*>
        parsePositive parseDouble g <*>
        parsePositive parseDouble b)) <*>
    (Vector <$> parseDouble x <*> parseDouble y <*> parseDouble z)
parseLight ("spherical":_) = Left $ missingString "spherical"
parseLight (l:_) = Left $ "Unknown light source type: \'" ++ l ++ "\'"

parseLights :: Parser [[String]] [LightSource RGB]
parseLights = parseList parseLight

findPropLine :: String -> [[String]] -> Maybe [String]
findPropLine p = (tail <$>) . find ((== p) . head)

parseProp :: Parser String a -> String -> [[String]] -> Either String a
parseProp f p xs = case findPropLine p xs of
    Just (x:_) -> f x
    Just [] -> Left $ "Missing property argument of \'" ++ p ++ "\'"
    Nothing -> Left $ "Missing property: \'" ++ p ++ "\'"

parseBgColor :: Parser [[String]] RGB
parseBgColor xs = case findPropLine "bgcolor" xs of
    Just (r:g:b:_) -> makeRGB <$>
        parsePositive parseDouble r <*>
        parsePositive parseDouble g <*>
        parsePositive parseDouble b
    Just _ -> Left "Missing property argument of \'bgRGB\'"
    Nothing -> return black

parseScene :: ([[String]], [[String]], [[String]]) -> Either String (Scene RGB)
parseScene (xs, l, o) = Scene <$>
    parseObjects o <*>
    parseLights l <*>
    parseProp (parsePositive parseInt) "imwidth" xs <*>
    parseProp (parsePositive parseInt) "imheight" xs <*>
    parseProp (parsePositive parseDouble) "scrwidth" xs <*>
    parseProp (parsePositive parseDouble) "scrheight" xs <*>
    parseBgColor xs <*>
    parseProp (parsePositive parseDouble) "depth" xs
