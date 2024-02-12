
import Data.List

preambleLength :: Int
preambleLength = 25

main :: IO ()
main = do
    numbers <- map read . lines <$> readFile "input.txt"
    let invalidNum = findInvalidNumber preambleLength numbers
    print invalidNum

findInvalidNumber :: Int -> [Int] -> Int
findInvalidNumber preambleLength numbers = 
    head [ n | (n, chunk) <- zip (drop preambleLength numbers) (chunksOf (preambleLength) numbers), not $ isValid n chunk]

isValid :: Int -> [Int] -> Bool
isValid number previousNumbers = any (\x -> (number-x) `elem` previousNumbers) previousNumbers

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop 1 xs)
