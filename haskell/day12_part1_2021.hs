import qualified Data.Map as Map
import Data.List (nub)
import Data.Char (isLower)
import System.IO (readFile)

type Graph = Map.Map String [String]

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let edges = map (split '-') $ lines contents
        graph = buildGraph edges
        paths = findAllPaths graph "start" []
    print $ length paths

split :: Char -> String -> (String, String)
split _ [] = ("", "")
split delim str = (takeWhile (/= delim) str, tail $ dropWhile (/= delim) str)

buildGraph :: [(String, String)] -> Graph
buildGraph = foldl (\acc (from, to) -> addEdge acc from to) Map.empty

addEdge :: Graph -> String -> String -> Graph
addEdge graph from to = Map.insertWith (++) from [to] $ Map.insertWith (++) to [from] graph

findAllPaths :: Graph -> String -> [String] -> [[String]]
findAllPaths graph current visited
    | current == "end" = [["end"]]
    | otherwise = concatMap (\next -> map (current:) $ findAllPaths graph next (current:visited)) nextNodes
    where nextNodes = filter (\node -> isBigCave node || not (node `elem` visited)) (Map.findWithDefault [] current graph)

isBigCave :: String -> Bool
isBigCave = not . all isLower