
{-# LANGUAGE BangPatterns #-} -- Can help with strictness in tight loops/folds

import System.IO (readFile)
import Data.List (foldl') -- Using strict fold for summation

-- --- Parsing Helpers ---

-- Helper: Split a list based on a predicate, removing the delimiter elements.
-- Example: splitBy (==',') "a,b,c" -> ["a", "b", "c"]
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy p xs = case break p xs of
                 (group, [])      -> [group]       -- No delimiter found, return the whole list as one group
                 (group, _:rest) -> group : splitBy p rest -- Delimiter found, add group and recurse on the rest

-- Helper: Split a string by a specific character.
splitOnChar :: Char -> String -> [String]
splitOnChar delim = splitBy (== delim)

-- Helper: Replace all occurrences of a substring with another string.
-- Used here to replace " or " with a single character ('|') to simplify splitting ranges.
replace :: String -> String -> String -> String
replace _ _ [] = [] -- Base case: empty haystack
replace needle replacement haystack@(x:xs) =
    if take (length needle) haystack == needle -- Check if the needle starts at the current position
    then replacement ++ replace needle replacement (drop (length needle) haystack) -- If yes, append replacement and recurse on the rest of haystack after the needle
    else x : replace needle replacement xs -- If no, keep the current character and recurse on the tail

-- Helper to parse a range string like "1-3" into an (Int, Int) tuple representing (min, max).
-- Assumes valid format "number-number". Uses `error` for invalid format.
parseRange :: String -> (Int, Int)
parseRange s = case break (== '-') s of
                 -- Ensure both parts are non-empty after splitting to avoid read errors on empty strings
                 (minStr@(_:_), '-':maxStr@(_:_)) -> (read minStr, read maxStr)
                 -- Handle cases where the format is incorrect
                 _ -> error $ "Invalid range format encountered: " ++ s

-- Helper to parse the ranges part of a rule, like "1-3 or 5-7".
-- Returns a list of (Int, Int) tuples.
parseRangesStr :: String -> [(Int, Int)]
parseRangesStr s = map parseRange $ splitOnChar '|' $ replace " or " "|" s -- Replace " or " with '|', then split by '|' and parse each resulting range string

-- Helper to parse a full rule line, e.g., "class: 1-3 or 5-7".
-- Extracts only the ranges, discarding the field name (which is not needed for Part 1).
parseRuleLine :: String -> [(Int, Int)]
parseRuleLine line =
    case break (== ':') line of
      -- Expects the format "field name: range1 or range2..."
      (_, ':':' ':rangesStr) -> parseRangesStr rangesStr -- Skip the field name and ": ", parse the rest
      -- Handle cases where the rule line format is incorrect
      _                      -> error $ "Invalid rule line format encountered: " ++ line

-- Helper to parse a comma-separated ticket line, e.g., "7,1,14".
-- Returns a list of Ints. Uses `error` if `read` fails on non-integer parts.
parseTicket :: String -> [Int]
parseTicket s = map read $ splitOnChar ',' s

-- --- Core Logic ---

-- Check if a number 'n' falls within any of the provided ranges [(min, max)].
-- The '!' BangPatterns encourage evaluation of 'n', 'minVal', and 'maxVal' before the comparison,
-- which might offer a minor performance benefit by ensuring values are evaluated strictly.
isValid :: Int -> [(Int, Int)] -> Bool
isValid !n ranges = any (\(!minVal, !maxVal) -> n >= minVal && n <= maxVal) ranges

-- --- Main Execution ---

main :: IO ()
main = do
    -- Read the entire content of "input.txt".
    content <- readFile "input.txt"
    let inputLines = lines content

    -- Split the input file lines into sections based on blank lines.
    -- Expected structure: [Rule lines], [Your ticket section], [Nearby tickets section]
    let sections = splitBy (== "") inputLines

    -- Basic validation: Ensure we have the expected number of sections (at least 3).
    if length sections < 3 then
        putStrLn "Error: Input file does not seem to contain the three required sections (rules, your ticket, nearby tickets) separated by blank lines."
    else do
        -- Section 0 contains the rules.
        let ruleLines = head sections -- Equivalent to sections !! 0
        -- Parse all rule lines to get a single flat list containing all valid ranges from all rules.
        -- The '!' here forces the evaluation of the entire list of ranges immediately.
        let !allRanges = concatMap parseRuleLine ruleLines

        -- Section 2 contains the nearby tickets, including the "nearby tickets:" header.
        let nearbyTicketLinesWithHeader = sections !! 2
        -- Check if the nearby tickets section is not empty and has the expected header.
        if null nearbyTicketLinesWithHeader || head nearbyTicketLinesWithHeader /= "nearby tickets:" then
            putStrLn "Error: Nearby tickets section is missing or does not start with 'nearby tickets:' header."
        else do
            -- Skip the header line to get only the ticket data lines.
            let nearbyTicketLines = tail nearbyTicketLinesWithHeader
            -- Parse each nearby ticket line into a list of numbers. This results in a list of lists: [[Int]].
            let nearbyTickets = map parseTicket nearbyTicketLines

            -- Calculate the ticket scanning error rate.
            -- 1. Flatten the list of nearby tickets into a single list containing all numbers from all nearby tickets.
            let allNearbyValues = concat nearbyTickets
            -- 2. Filter this list, keeping only the numbers that are *not* valid according to *any* of the ranges defined in `allRanges`.
            let invalidValues = filter (\value -> not (isValid value allRanges)) allNearbyValues
            -- 3. Sum the collected invalid values using `foldl'` (a strict left fold) for potentially better performance on large lists compared to the lazy `sum`.
            let !errorRate = foldl' (+) 0 invalidValues

            -- Print the final calculated error rate to standard output.
            print errorRate
