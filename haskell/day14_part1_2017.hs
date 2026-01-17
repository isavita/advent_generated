
import Data.Char (ord)
import Data.Bits (xor, popCount)
import Data.List (foldl')

{-
    Advent of Code - Day 14: Disk Defragmentation
    The task is to count the number of used squares in a 128x128 grid.
    The grid is generated row by row using the Knot Hash algorithm.
-}

-- Helper function to split a list into chunks of size n.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Performs a single step of the Knot Hash reversal.
-- To efficiently handle the circular list, we rotate the list so the current 
-- position is at the start, perform the reversal, and rotate it back.
step :: ([Int], Int, Int) -> Int -> ([Int], Int, Int)
step (ls, p, s) len = (nextList, (p + len + s) `mod` 256, s + 1)
  where
    n = 256
    rotated = drop p ls ++ take p ls
    reversed = reverse (take len rotated) ++ drop len rotated
    -- Shift the list back to its original orientation.
    nextList = drop (n - p) reversed ++ take (n - p) reversed

-- Computes the Knot Hash of a string and returns it as a list of 16 integers (bytes).
knotHash :: String -> [Int]
knotHash input = dense
  where
    -- ASCII values of input + standard length suffix.
    lengths = map ord input ++ [17, 31, 73, 47, 23]
    initialState = ([0..255], 0, 0)
    
    -- Perform 64 rounds of shuffling to create the sparse hash.
    (sparse, _, _) = iterate (\curr -> foldl' step curr lengths) initialState !! 64
    
    -- XOR chunks of 16 elements from the sparse hash to create the dense hash.
    dense = map (foldr1 xor) (chunksOf 16 sparse)

-- Solves Part 1: Counts the total number of 'used' squares (set bits) in the grid.
solve :: String -> Int
solve key = sum [countBitsInRow r | r <- [0..127]]
  where
    -- Each row's key is the input key followed by a dash and the row index.
    countBitsInRow i = sum . map popCount $ knotHash (key ++ "-" ++ show i)

main :: IO ()
main = do
    -- Standard puzzle input handling: read from input.txt and trim whitespace.
    content <- readFile "input.txt"
    let key = head (words content)
    
    -- Calculate and print the result.
    let result = solve key
    print result

{- 
    Implementation Details:
    1. The Knot Hash logic is consistent with the Day 10 challenge.
    2. Rotation logic (drop/take) is O(N). Since N is constant (256), the complexity 
       of calculating one row is approximately O(Rounds * Lengths), which is very efficient.
    3. popCount from Data.Bits provides an optimized way to count set bits in an integer.
-}

