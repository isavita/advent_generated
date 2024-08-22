import qualified Data.Set as Set
import qualified Data.Map as Map
import System.IO

main = do
    contents <- readFile "input.txt"
    let connections = parseInput contents
    let group = findGroup connections 0
    print $ Set.size group

parseInput :: String -> Map.Map Int [Int]
parseInput input = Map.fromList $ map parseLine (lines input)
    where
        parseLine line = let (program, rest) = break (== ' ') line
                             programId = read program
                             connectedIds = map read $ words $ map (\c -> if c == ',' then ' ' else c) (drop 4 rest)
                         in (programId, connectedIds)

findGroup :: Map.Map Int [Int] -> Int -> Set.Set Int
findGroup connections start = go Set.empty [start]
    where
        go visited [] = visited
        go visited (x:xs)
            | x `Set.member` visited = go visited xs
            | otherwise = let newVisited = Set.insert x visited
                              neighbors = Map.findWithDefault [] x connections
                          in go newVisited (xs ++ neighbors)