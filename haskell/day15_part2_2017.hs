import System.IO
import Data.Bits

main = do
    contents <- readFile "input.txt"
    let [startA, startB] = map read $ lines contents
    let factorA = 16807
    let factorB = 48271
    let divisor = 2147483647
    let count = length $ filter (== True) $ take 5000000 $ judge (generator startA factorA 4 divisor) (generator startB factorB 8 divisor)
    print count

generator :: Int -> Int -> Int -> Int -> [Int]
generator start factor multiple divisor = filter (\x -> x `mod` multiple == 0) $ iterate (\x -> (x * factor) `mod` divisor) start

judge :: [Int] -> [Int] -> [Bool]
judge genA genB = map (\(a, b) -> (a .&. 0xFFFF) == (b .&. 0xFFFF)) $ zip genA genB