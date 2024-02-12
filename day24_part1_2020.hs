
{-# LANGUAGE LambdaCase #-}

import System.IO
import qualified Data.Map as Map

data Coordinate = Coordinate {q :: Int, r :: Int} deriving (Eq, Ord)

directions = Map.fromList [
    ("e",  Coordinate 1 0),
    ("se", Coordinate 0 1),
    ("sw", Coordinate (-1) 1),
    ("w",  Coordinate (-1) 0),
    ("nw", Coordinate 0 (-1)),
    ("ne", Coordinate 1 (-1))
    ]

parseDirections :: String -> [String]
parseDirections [] = []
parseDirections (x:xs) 
    | x `elem` "ew" = [x] : parseDirections xs
    | otherwise = [x, head xs] : parseDirections (tail xs)

move :: Coordinate -> Coordinate -> Coordinate
move (Coordinate q1 r1) (Coordinate q2 r2) = Coordinate (q1 + q2) (r1 + r2)

main = do
    file <- openFile "input.txt" ReadMode
    contents <- hGetContents file
    let input = lines contents
    let blackTiles = foldl (\tiles line -> 
                            let coord = foldl move (Coordinate 0 0) (map (directions Map.!) (parseDirections line))
                            in Map.alter (\case {Nothing -> Just True; Just b -> Just (not b)}) coord tiles
                        ) Map.empty input
    print $ length $ filter id $ Map.elems blackTiles
