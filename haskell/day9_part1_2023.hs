
import System.IO

parseInput :: [String] -> [[Int]]
parseInput = map (map read . words)

allZeros :: [Int] -> Bool
allZeros = all (== 0)

calculateExtrapolation :: [Int] -> [Int]
calculateExtrapolation history = zipWith (-) (tail history) history

calculateExtrapolations :: [Int] -> [[Int]]
calculateExtrapolations history = takeWhile (not . allZeros) $ iterate calculateExtrapolation history

solve :: [[Int]] -> Int
solve histories = sum [sum (map last (calculateExtrapolations history)) | history <- histories]

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    print . solve . parseInput $ input
