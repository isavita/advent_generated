
import Data.List
import Data.Maybe

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let banks = map read $ words contents :: [Int]
    let seen = [] :: [[Int]]
    let cycles = 0
    let result = redistributionLoop banks seen cycles
    putStrLn $ "It takes " ++ show result ++ " redistribution cycles to reach a repeated configuration."

redistributionLoop :: [Int] -> [[Int]] -> Int -> Int
redistributionLoop banks seen cycles
    | state `elem` seen = cycles
    | otherwise = redistributionLoop newBanks (banks : seen) (cycles + 1)
    where state = banks
          maxIndex = fromJust $ elemIndex (maximum banks) banks
          blocks = banks !! maxIndex
          newBanks = redistributeBlocks maxIndex (banks // [(maxIndex, 0)]) blocks

redistributeBlocks :: Int -> [Int] -> Int -> [Int]
redistributeBlocks _ banks 0 = banks
redistributeBlocks index banks blocks = redistributeBlocks newIndex newBanks (blocks - 1)
    where newIndex = (index + 1) `mod` length banks
          newBanks = banks // [(newIndex, (banks !! newIndex) + 1)]

(//) :: [a] -> [(Int, a)] -> [a]
(//) list updates = foldl (\acc (i, v) -> take i acc ++ [v] ++ drop (i + 1) acc) list updates
