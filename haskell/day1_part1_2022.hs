
import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let maxCalories = maximum $ map sum $ splitOnEmptyLines $ lines contents
    print maxCalories

splitOnEmptyLines :: [String] -> [[Int]]
splitOnEmptyLines = foldr f [[]]
  where f "" acc = [] : acc
        f x (y:ys) = (read x : y) : ys
