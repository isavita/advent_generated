
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (scanl') -- Use strict scanl' for potentially better performance

-- | Parses the input string into a list of frequency changes (Ints).
-- Handles lines like "+5", "-10", "3".
parseInput :: String -> [Int]
parseInput = map readInt . lines
  where
    readInt :: String -> Int
    -- Explicitly handle the '+' sign, although `read` might handle it too.
    -- This ensures robustness.
    readInt ('+':s) = read s
    readInt s       = read s

-- | Solves Part 1: Calculates the final frequency after all changes.
solvePart1 :: [Int] -> Int
solvePart1 = sum -- Summing all changes gives the final frequency relative to the start (0).

-- | Solves Part 2: Finds the first frequency reached twice.
-- It iterates through the cumulative frequencies, keeping track of seen frequencies
-- in a Set until a duplicate is found.
solvePart2 :: [Int] -> Int
solvePart2 changes = findDuplicate Set.empty frequencySequence
  where
    -- Create an infinite list of changes by cycling the input list.
    infiniteChanges :: [Int]
    infiniteChanges = cycle changes

    -- Calculate the sequence of frequencies reached, starting from 0.
    -- scanl' is used for efficiency (strict accumulation).
    -- Example: scanl' (+) 0 [1, -2, 3] == [0, 1, -1, 2]
    frequencySequence :: [Int]
    frequencySequence = scanl' (+) 0 infiniteChanges

    -- Helper function to find the first duplicate in a list using a Set.
    findDuplicate :: Set Int -> [Int] -> Int
    findDuplicate seen (f:fs)
      | Set.member f seen = f                     -- If f is already in the set, we found the duplicate.
      | otherwise         = findDuplicate (Set.insert f seen) fs -- Otherwise, add f to the set and continue.
    -- This case should theoretically not be reached with an infinite list,
    -- but it's good practice for totality checking if the input list could be empty
    -- (though cycle [] would loop indefinitely).
    findDuplicate _ []    = error "No duplicate found or empty frequency sequence - Should not happen with AoC input"

-- | Main entry point of the program.
main :: IO ()
main = do
    -- Read the content of the input file.
    content <- readFile "input.txt"

    -- Parse the input file content into a list of integers.
    let changes = parseInput content

    -- Calculate and print the result for Part 1.
    let result1 = solvePart1 changes
    putStrLn $ "Part 1: " ++ show result1

    -- Calculate and print the result for Part 2.
    let result2 = solvePart2 changes
    putStrLn $ "Part 2: " ++ show result2

