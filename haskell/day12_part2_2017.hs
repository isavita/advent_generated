import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO

-- Parse the input file to create a graph representation
parseInput :: FilePath -> IO (Map.Map Int [Int])
parseInput filename = do
    contents <- readFile filename
    let connections = Map.fromList $ map parseLine (lines contents)
    return connections
  where
    parseLine line = 
        let (program:_:targets) = words line
            programId = read program
            targetIds = map (read . filter (/= ',')) targets
        in (programId, targetIds)

-- Perform a DFS to find all programs in the same group
dfs :: Map.Map Int [Int] -> Int -> Set.Set Int -> Set.Set Int
dfs graph start visited
    | Set.member start visited = visited
    | otherwise = foldl (\v n -> dfs graph n v) (Set.insert start visited) (Map.findWithDefault [] start graph)

-- Count the number of groups
countGroups :: Map.Map Int [Int] -> Int
countGroups graph = go graph Set.empty 0
  where
    go g visited count
        | Map.null g = count
        | otherwise =
            let (program, _) = Map.findMin g
                newVisited = dfs g program visited
                remainingGraph = Map.filterWithKey (\k _ -> not (Set.member k newVisited)) g
            in go remainingGraph newVisited (count + 1)

main :: IO ()
main = do
    graph <- parseInput "input.txt"
    let totalGroups = countGroups graph
    print totalGroups