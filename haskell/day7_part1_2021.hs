
import Data.List
import Data.Function

main = do
    contents <- readFile "input.txt"
    let positions = sort $ concatMap (map read . words . map (\c -> if c == ',' then ' ' else c)) $ lines contents
    let minFuel = minimum [sum [myAbs (p - i) | p <- positions] | i <- [head positions .. last positions]]
    print minFuel

myAbs n = if n < 0 then -n else n
