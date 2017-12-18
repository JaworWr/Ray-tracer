module SceneParser(parseScene, ParseData) where

import Scene(Object)

data ParseData = ParseData {
    imWidth :: Int,
    imHeight :: Int,
    scene :: [Object]
}

parseHeader :: [[String]] -> Either String ([[String]], Int, Int)
parseHeader ((w:h:_):xs) = case (reads w, reads h) of
    ([(width, [])], [(height, [])]) -> return (xs, width, height)
    _ -> Left "Wrong header"
parseHeader _ = Left "Image dimensions missing"

parseLines :: [[String]] -> Either String ParseData
parseLines = undefined

parseScene :: String -> Either String ParseData
parseScene = parseLines . map words . lines
