
import System.IO
import Data.List (sort)

-- Function to calculate the differences
calculateDifferences :: [Int] -> (Int, Int)
calculateDifferences adapters = foldl countDifferences (0, 0) differences
  where
    differences = zipWith (-) (tail sortedAdapters) sortedAdapters
    sortedAdapters = 0 : (sort adapters) ++ [maximum adapters + 3]
    countDifferences (ones, threes) diff
      | diff == 1 = (ones + 1, threes)
      | diff == 3 = (ones, threes + 1)
      | otherwise = (ones, threes)

main :: IO ()
main = do
    -- Read the input from the file
    contents <- readFile "input.txt"
    let adapters = map read (lines contents) :: [Int]
    
    -- Calculate the differences
    let (ones, threes) = calculateDifferences adapters
    
    -- Print the result
    print $ ones * threes
