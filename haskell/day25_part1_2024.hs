
import Data.List (transpose)

parseLock :: [String] -> [Int]
parseLock b = map (\c -> length $ takeWhile (== '#') $ map (!! c) (drop 1 b)) [0..4]

parseKey :: [String] -> [Int]
parseKey b = map (\c -> length $ takeWhile (== '#') $ map (!! c) (reverse $ take 6 b)) [0..4]

fits :: [Int] -> [Int] -> Bool
fits lock key = all (<= 5) $ zipWith (+) lock key

solve :: [String] -> Int
solve raw
    | length raw `mod` 7 /= 0 = 0
    | otherwise = length [() | lock <- locks, key <- keys, fits lock key]
    where
        blocks = map (take 7) $ takeWhile (not . null) $ iterate (drop 7) raw
        locks = map parseLock $ filter ((all (== '#')) . head) blocks
        keys  = map parseKey  $ filter ((not . all (== '#')) . head) blocks
main :: IO ()
main = do
    raw <- lines <$> readFile "input.txt"
    let input = filter (not . null) raw
    print $ solve input
