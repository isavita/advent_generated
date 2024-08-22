import qualified Data.Map as Map
import Data.Char (isLower)
import Data.List (nub)
import System.IO (readFile)

type Graph = Map.Map String [String]

main = do
    contents <- readFile "input.txt"
    let connections = map (splitOn '-') $ lines contents
        graph = buildGraph connections
        paths = findAllPaths graph "start" [] False
    print $ length paths

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delimiter str = let (before, remainder) = span (/= delimiter) str
                        in before : case remainder of
                            [] -> []
                            _:after -> splitOn delimiter after

buildGraph :: [[String]] -> Graph
buildGraph connections = foldl addConnection Map.empty connections
    where addConnection graph [from, to] = Map.insertWith (++) from [to] $ Map.insertWith (++) to [from] graph

findAllPaths :: Graph -> String -> [String] -> Bool -> [[String]]
findAllPaths graph current visited smallCaveVisitedTwice
    | current == "end" = [["end"]]
    | otherwise = concatMap extendPath nextCaves
    where nextCaves = Map.findWithDefault [] current graph
          extendPath next
            | next == "start" = []
            | isLower (head next) && next `elem` visited = if smallCaveVisitedTwice then [] else map (next:) $ findAllPaths graph next (next:visited) True
            | otherwise = map (next:) $ findAllPaths graph next (next:visited) smallCaveVisitedTwice