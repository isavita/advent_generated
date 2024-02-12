
import System.IO

parseInput :: [String] -> [[Int]]
parseInput = map (map read . words)

allZeros :: [Int] -> Bool
allZeros = all (==0)

calculateExtrapolation :: [Int] -> [Int]
calculateExtrapolation history = zipWith (-) (tail history) history

calculateExtrapolations :: [Int] -> [[Int]]
calculateExtrapolations history = takeWhile (not . allZeros) $ iterate calculateExtrapolation history

solve :: [String] -> Int
solve input = sum $ map (calculatePrediction . calculateExtrapolations . map read . words) input
  where
    calculatePrediction [] = 0
    calculatePrediction extrapolations = foldr (\x acc -> head x - acc) 0 extrapolations

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    print $ solve input
