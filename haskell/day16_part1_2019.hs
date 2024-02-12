
import Data.Char

main = do
    contents <- readFile "input.txt"
    let input = map digitToInt contents
    let result = foldl (\digits _ -> applyFFT digits) input [1..100]
    putStrLn $ map intToDigit $ take 8 result

applyFFT :: [Int] -> [Int]
applyFFT input = map (\i -> abs(sum $ zipWith (*) input (pattern i)) `mod` 10) [0..length input - 1]
    where
        basePattern = [0, 1, 0, -1]
        pattern i = drop 1 $ cycle $ concatMap (replicate (i + 1)) basePattern
