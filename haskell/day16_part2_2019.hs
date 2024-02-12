
import Data.Char

main = do
    contents <- readFile "input.txt"
    let input = head $ lines contents
        repeatedInput = concat $ replicate 10000 (map digitToInt input)
        offset = read (take 7 input) :: Int
        finalSignal = foldr (\_ acc -> scanr1 (\x y -> (x + y) `mod` 10) acc) repeatedInput [1..100]
        message = map intToDigit $ take 8 $ drop offset finalSignal
    putStrLn message
