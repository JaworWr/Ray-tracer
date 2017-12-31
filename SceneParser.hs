module SceneParser (parse) where

import Scene
import Geometry
import DataTypes

import Data.Char
import Data.List

parse :: String -> Either String Scene
parse = parseScene . getHeader . splitInput

missingString :: String -> String
missingString s = "Missing arguments for \'" ++ s ++ "\'"

splitInput :: String -> [[String]]
splitInput = map words
    . filter (\s -> not (null s) && head s /= '#')
    . lines . map toLower

getHeader :: [[String]] -> ([[String]], [[String]])
getHeader [] = ([], [])
getHeader (("objects":_):xs) = ([], xs)
getHeader (l:xs) = let (ys, zs) = getHeader xs in (l:ys, zs)

parseDouble :: String -> Either String Double
parseDouble s = case reads s of
    [(x, [])] -> return x
    _ -> Left "Expected a Double"

parseInt :: String -> Either String Int
parseInt s = case reads s of
    [(x, [])] -> return x
    _ -> Left "Expected an Int"

parseSurface :: [String] -> Either String Surface
parseSurface ("diffusive":r:g:b:_) = Diffusive <$>
    (RGB <$> parseDouble r <*> parseDouble g <*> parseDouble b)
parseSurface ("diffusive":_) = Left $ missingString "diffusive"
parseSurface (s:_) = Left $ "Unknown surface type: \'" ++ s ++ "\'"
parseSurface [] = Left "Missing surface type"

parseObject :: [String] -> Either String Object
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

parseObjects :: [[String]] -> Either String [Object]
parseObjects = foldl (\acc l -> (:) <$> parseObject l <*> acc) (return [])

findPropLine :: String -> [[String]] -> Maybe [String]
findPropLine p = (tail <$>) . find ((== p) . head)

parseLightSource :: [[String]] -> Either String LightSource
parseLightSource xs = case findPropLine "light" xs of
    Just ("directional":x:y:z:_) -> makeDirectional <$>
        (Vector <$> parseDouble x <*> parseDouble y <*> parseDouble z)
    Just ("directional":_) -> Left $ missingString "directional"
    Just (s:_) -> Left $ "Unknown light source type: \'" ++ s ++ "\'"
    Just [] -> Left "Light source information missing"
    Nothing -> Left "Light source information missing"

parseProp :: (String -> Either String a) -> String -> [[String]] -> Either String a
parseProp f p xs = case findPropLine p xs of
    Just (x:_) -> f x
    Just [] -> Left $ "Missing property arguments of \'" ++ p ++ "\'"
    Nothing -> Left $ "Missing property: \'" ++ p ++ "\'"

parseScene :: ([[String]], [[String]]) -> Either String Scene
parseScene (xs, o) = Scene <$>
    parseObjects o <*>
    parseLightSource xs <*>
    parseProp parseInt "imwidth" xs <*>
    parseProp parseInt "imheight" xs <*>
    parseProp parseDouble "scrwidth" xs <*>
    parseProp parseDouble "scrheight" xs <*>
    parseProp parseDouble "depth" xs
