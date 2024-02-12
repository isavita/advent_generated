
import Data.List
import Data.Maybe

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let adapters = sort $ map read $ lines contents :: [Int]
    let arrangements = countArrangements (0:adapters ++ [last adapters + 3])
    print arrangements

countArrangements :: [Int] -> Int
countArrangements adapters = fromJust $ lookup (last adapters) ways
  where
    ways = foldl calculateWays [(0, 1)] (tail adapters)
    calculateWays waysList joltage = (joltage, sum $ map snd $ filter (\(prev, _) -> joltage - prev <= 3) waysList) : waysList
