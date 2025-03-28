
import Data.Bits (xor)
import System.IO
import Data.List (tails, isPrefixOf, findIndex, foldl', maximum)
import qualified Data.Map.Strict as Map
import Control.Monad (replicateM)
import Data.Maybe (mapMaybe)

-- --- Constants ---

moduloValue :: Int
moduloValue = 16777216 -- 2^24

numGenerations :: Int
numGenerations = 2000

changeSequenceLength :: Int
changeSequenceLength = 4

-- --- Core Logic: Secret Number Generation ---

-- Prune the secret number: secret `mod` 2^24
prune :: Int -> Int
prune n = n `mod` moduloValue

-- Mix a value into the secret number: value `xor` secret
mix :: Int -> Int -> Int
mix val sec = val `xor` sec

-- Calculate the next secret number in the sequence
nextSecret :: Int -> Int
nextSecret sec0 =
    let -- Step 1: Multiply by 64, mix, prune
        sec1 = prune (mix (sec0 * 64) sec0)
        -- Step 2: Divide by 32 (integer division), mix, prune
        sec2 = prune (mix (sec1 `div` 32) sec1)
        -- Step 3: Multiply by 2048, mix, prune
        sec3 = prune (mix (sec2 * 2048) sec2)
    in sec3

-- Generate a sequence of N+1 secrets (initial + N generated)
generateSecrets :: Int -> Int -> [Int]
generateSecrets n initial = take (n + 1) (iterate nextSecret initial)

-- --- Part 1 Logic ---

-- Get the Nth generated secret number (which is at index N in the 0-indexed list)
getNthSecret :: Int -> Int -> Int
getNthSecret n initialSecret = (iterate nextSecret initialSecret) !! n

solvePart1 :: [Int] -> Int
solvePart1 initialSecrets = sum $ map (getNthSecret numGenerations) initialSecrets

-- --- Part 2 Logic ---

-- Get the price (last digit) from a secret number
getPrice :: Int -> Int
getPrice secret = secret `mod` 10

-- Get the sequence of prices from a sequence of secrets
getPrices :: [Int] -> [Int]
getPrices = map getPrice

-- Get the sequence of price changes from a sequence of prices
getChanges :: [Int] -> [Int]
getChanges prices = zipWith (-) (tail prices) prices

-- Data structure to hold processed data for a single buyer
-- We only need prices and changes for Part 2 calculations
type BuyerData = ([Int], [Int]) -- (prices, changes)

-- Process an initial secret to get the relevant buyer data for Part 2
processBuyer :: Int -> BuyerData
processBuyer initial =
    let secrets = generateSecrets numGenerations initial
        prices = getPrices secrets
        changes = getChanges prices
    in (prices, changes)

-- Find the first occurrences of all change subsequences of a given length for a single buyer
-- Returns a Map where Key = ChangeSubsequence, Value = Price at the end of that subsequence
type FirstOccurrences = Map.Map [Int] Int

findFirstOccurrences :: BuyerData -> FirstOccurrences
findFirstOccurrences (prices, changes) = go 0 Map.empty
  where
    n = length changes
    go :: Int -> FirstOccurrences -> FirstOccurrences
    go idx accMap
        -- Stop if the remaining changes are fewer than the required sequence length
        | idx > n - changeSequenceLength = accMap
        | otherwise =
            let subSeq = take changeSequenceLength (drop idx changes)
            in -- Check if we've already found this subsequence earlier for this buyer
               if Map.member subSeq accMap then
                   -- If yes, continue searching from the next index
                   go (idx + 1) accMap
               else
                   -- If no, this is the first occurrence. Record the price.
                   -- The price corresponds to the *end* of the subsequence.
                   -- Subsequence starts at changes[idx], involves prices[idx+1]...prices[idx+changeSequenceLength]
                   -- The relevant price is prices[idx + changeSequenceLength]
                   let price = prices !! (idx + changeSequenceLength)
                       accMap' = Map.insert subSeq price accMap
                   -- Continue searching from the next index
                   in go (idx + 1) accMap'

-- Aggregate results from all buyers. For each possible change sequence, sum the prices
-- obtained from the *first* occurrence for each buyer who experiences that sequence.
aggregateAllBuyers :: [FirstOccurrences] -> Map.Map [Int] Int
aggregateAllBuyers buyerMaps =
    foldl' aggregateBuyer Map.empty buyerMaps
  where
    -- Merges one buyer's first occurrence map into the global totals map
    aggregateBuyer :: Map.Map [Int] Int -> FirstOccurrences -> Map.Map [Int] Int
    aggregateBuyer currentTotals buyerMap =
        -- For each (sequence, price) pair in the buyer's map, add the price
        -- to the total for that sequence in the global map.
        Map.foldlWithKey' (\acc seq price -> Map.insertWith (+) seq price acc) currentTotals buyerMap

solvePart2 :: [Int] -> Int
solvePart2 initialSecrets =
    -- 1. Process each buyer to get their prices and changes
    let allBuyerData = map processBuyer initialSecrets
    -- 2. For each buyer, find the price associated with the first occurrence of each 4-change sequence
        firstOccurrencesPerBuyer = map findFirstOccurrences allBuyerData
    -- 3. Aggregate these results: for each 4-change sequence, sum the prices across all buyers
        aggregatedResults = aggregateAllBuyers firstOccurrencesPerBuyer
    -- 4. Find the maximum total banana count among all possible sequences
    in if Map.null aggregatedResults
       then 0 -- Handle cases where no sequences might be found (unlikely for puzzle input)
       else maximum (Map.elems aggregatedResults)


-- --- Main Execution ---

main :: IO ()
main = do
    -- Read input file
    contents <- readFile "input.txt"
    -- Parse initial secret numbers
    let initialSecrets = map read (lines contents) :: [Int]

    -- --- Solve Part 1 (commented out as only Part 2 is requested by the final prompt) ---
    -- let resultPart1 = solvePart1 initialSecrets
    -- putStrLn $ "Part 1: " ++ show resultPart1 -- Expected: 17724064040

    -- --- Solve Part 2 ---
    let resultPart2 = solvePart2 initialSecrets
    -- Print the result for Part 2
    print resultPart2

