import System.IO
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

type Point = (Int, Int)
type WirePath = [String]

main = do
    contents <- readFile "input.txt"
    let wirePaths = map (splitOn ',') (lines contents)
        wire1 = traceWire (0, 0) wirePaths !! 0
        wire2 = traceWire (0, 0) wirePaths !! 1
        intersections = Map.keysSet wire1 `Set.intersection` Map.keysSet wire2
        minSteps = minimum [wire1 Map.! p + wire2 Map.! p | p <- Set.toList intersections]
    print minSteps

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delim str = let (first, rest) = break (== delim) str
                    in first : case rest of
                                [] -> []
                                (_:xs) -> splitOn delim xs

traceWire :: Point -> [WirePath] -> [Map.Map Point Int]
traceWire _ [] = []
traceWire start (path:paths) = let wireMap = tracePath start path 0 Map.empty
                               in wireMap : traceWire start paths

tracePath :: Point -> WirePath -> Int -> Map.Map Point Int -> Map.Map Point Int
tracePath _ [] _ wireMap = wireMap
tracePath (x, y) (move:moves) steps wireMap = let (dir:distStr) = move
                                                  dist = read distStr
                                                  (dx, dy) = case dir of
                                                              'U' -> (0, 1)
                                                              'D' -> (0, -1)
                                                              'L' -> (-1, 0)
                                                              'R' -> (1, 0)
                                                  newWireMap = foldl' (\acc i -> let newPoint = (x + i*dx, y + i*dy)
                                                                                 in Map.insertWith (\_ old -> old) newPoint (steps + i) acc) wireMap [1..dist]
                                              in tracePath (x + dist*dx, y + dist*dy) moves (steps + dist) newWireMap