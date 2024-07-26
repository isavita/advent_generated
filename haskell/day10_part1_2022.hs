
import Data.List (foldl')
import System.IO (readFile)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let instructions = lines content
        x = foldl' process [1] instructions
        sum = foldl' (\acc (i, val) -> if (i - 19) `mod` 40 == 0 then acc + (i + 1) * val else acc) 0 (zip [0..] x)
    print sum

process :: [Int] -> String -> [Int]
process xs "noop" = xs ++ [last xs]
process xs line = let n = read (drop 5 line) in xs ++ [last xs, last xs + n]
