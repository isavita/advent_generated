
import System.IO
import Data.List
import Data.Char

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let [start, end] = map (read :: String -> Int) $ words $ map (\c -> if c == '-' then ' ' else c) contents
    print $ length [x | x <- [start..end], hasDoubleAndIncreasingDigits (show x)]

hasDoubleAndIncreasingDigits :: String -> Bool
hasDoubleAndIncreasingDigits s = any ((>1) . length) (group s) && all (uncurry (<=)) (zip s (tail s))
