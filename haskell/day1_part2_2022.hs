import Data.List
import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let elves = map (sum . map read) $ splitOnEmptyLines $ lines contents
        topThree = take 3 $ sortBy (flip compare) elves
    print $ sum topThree

splitOnEmptyLines :: [String] -> [[String]]
splitOnEmptyLines = foldr f [[]]
    where f "" acc = [] : acc
          f x (y:ys) = (x : y) : ys