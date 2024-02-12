
import Data.Bits
import Data.List

main = do
    contents <- readFile "input.txt"
    let [genAStart, genBStart] = map read $ lines contents
        genAFactor :: Int
        genAFactor = 16807
        genBFactor :: Int
        genBFactor = 48271
        modulus :: Int
        modulus = 2147483647
        numPairs = 40000000
        matches = length $ filter (\(a, b) -> a .&. 0xFFFF == b .&. 0xFFFF) $ take numPairs $ iterate (\(a, b) -> (a * genAFactor `mod` modulus, b * genBFactor `mod` modulus)) (genAStart, genBStart)
    print matches
