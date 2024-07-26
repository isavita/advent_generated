
import System.IO

-- Function to count trees encountered for a given slope
countTrees :: [[Char]] -> Int -> Int -> Int
countTrees forest right down = go 0 0 0
  where
    rows = length forest
    cols = length (head forest)
    go x y count
      | y >= rows = count  -- Stop if we go past the bottom
      | otherwise = go (x + right) (y + down) newCount
      where
        newCount = if forest !! y !! (x `mod` cols) == '#' then count + 1 else count

-- Main function to read input and calculate results
main :: IO ()
main = do
    -- Read the input file
    contents <- readFile "input.txt"
    let forest = lines contents

    -- Define the slopes to check
    let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

    -- Count trees for each slope
    let treeCounts = [countTrees forest right down | (right, down) <- slopes]

    -- Print the number of trees encountered for each slope
    mapM_ print treeCounts

    -- Calculate and print the product of the tree counts
    let productOfTrees = product treeCounts
    print productOfTrees
