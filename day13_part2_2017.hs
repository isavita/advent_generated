
import System.IO

data Scanner = Scanner { range :: Int, position :: Int, direction :: Int }

main = do
    contents <- readFile "input.txt"
    let input = map (map read . words . map (\c -> if c == ':' then ' ' else c)) $ lines contents
        firewall = map (\[depth, rng] -> (depth, Scanner rng 0 1)) input
        delay = findDelay firewall 0
    print delay

findDelay :: [(Int, Scanner)] -> Int -> Int
findDelay firewall delay
    | passThrough firewall delay = delay
    | otherwise = findDelay firewall (delay + 1)

passThrough :: [(Int, Scanner)] -> Int -> Bool
passThrough firewall delay = all (\(depth, scanner) -> (depth + delay) `mod` (2 * (range scanner - 1)) /= 0) firewall
