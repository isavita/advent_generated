
import System.IO (readFile)
import Data.List (intersect)
import qualified Data.Set as Set -- Using Set for efficient lookups

-- | Main entry point. Reads input, processes cards, and prints the total score.
main :: IO ()
main = do
    input <- readFile "input.txt"
    -- Process each line, calculate its score, and sum the results
    let totalScore = sum . map processCard . lines $ input
    print totalScore

-- | Parses a single line (card) and calculates its score.
processCard :: String -> Int
processCard line =
    let -- 1. Find and drop the "Card N: " prefix
        -- Find the index of ':'
        -- Assumes ':' is always present after the card number
        Just colonIndex = elemIndex ':' line
        -- Get the part after ": "
        cardData = drop (colonIndex + 2) line -- +2 to skip ": "

        -- 2. Split into winning numbers and your numbers at '|'
        -- Find the index of '|'
        -- Assumes '|' is always present separating the number lists
        Just pipeIndex = elemIndex '|' cardData
        -- Extract the strings for winning and your numbers
        winningStr = take pipeIndex cardData
        -- Add 2 to skip "| "
        yourStr    = drop (pipeIndex + 2) cardData

        -- 3. Parse the number strings into lists/sets of Ints
        -- Parse winning numbers into a Set for efficient lookup (O(log n))
        winningNums = Set.fromList . map read . words $ winningStr :: Set.Set Int
        -- Parse your numbers into a list
        yourNums    = map read . words $ yourStr    :: [Int]

        -- 4. Count matches
        -- Filter your numbers to find those present in the winning set
        matchCount = length $ filter (`Set.member` winningNums) yourNums

        -- 5. Calculate points based on the number of matches
        points = calculatePoints matchCount

    in points

-- | Calculates the points for a card based on the number of matches.
-- 0 matches = 0 points
-- 1 match = 1 point
-- 2 matches = 2 points
-- 3 matches = 4 points
-- n matches = 2^(n-1) points (for n > 0)
calculatePoints :: Int -> Int
calculatePoints 0 = 0
calculatePoints n = 2 ^ (n - 1)

-- | Helper function to find the first index of an element in a list.
-- Returns Nothing if the element is not found.
-- (Available in Data.List, but defined here for clarity if not importing all of Data.List)
elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex _ [] = Nothing
elemIndex x (y:ys)
    | x == y    = Just 0
    | otherwise = fmap (+1) (elemIndex x ys)

-- Example usage (assuming input.txt exists):
-- Compile: ghc -O2 -main-is Scratchcards Scratchcards.hs
-- Run: ./Scratchcards < input.txt
