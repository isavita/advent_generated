
import System.IO (readFile)
import Data.List (sort, group, sortOn)
import Data.Ord (Down(..), comparing)
import Data.Char (isDigit, digitToInt)
import Data.Function (on)

-- Define the types of hands, ordered from weakest to strongest
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
              deriving (Eq, Ord, Enum, Show, Bounded)

-- Define a structure to hold hand information for easier sorting
data HandInfo = HandInfo {
    hiHand :: String,    -- Original hand string "T55J5"
    hiBid :: Int,       -- Corresponding bid
    hiType :: HandType, -- Calculated type (e.g., FourOfAKind)
    hiCardValues :: [Int] -- Card values for tie-breaking (e.g., [10, 5, 5, 1, 5])
} deriving (Eq, Show)

-- Custom Ord instance for HandInfo based on Camel Cards rules
instance Ord HandInfo where
    compare = comparing hiType <> comparing hiCardValues
        where
             -- <> for Ordering comes from Data.Semigroup (implicitly imported via GHC base)
             -- It chains comparisons: if the first is EQ, it uses the second.

-- Function to parse a single line "HAND BID" into (String, Int)
parseLine :: String -> (String, Int)
parseLine line = case words line of
                   [hand, bidStr] -> (hand, read bidStr)
                   _              -> error $ "Invalid line format: " ++ line

-- Function to calculate the value of a card based on the rules (Part 1 or Part 2)
-- Part 1: J = 11
-- Part 2: J = 1
cardValue :: Int -> Char -> Int
cardValue jokerValue c
    | c == 'A'  = 14
    | c == 'K'  = 13
    | c == 'Q'  = 12
    | c == 'J'  = jokerValue
    | c == 'T'  = 10
    | isDigit c = digitToInt c
    | otherwise = error $ "Invalid card: " ++ [c]

-- Helper to determine hand type from sorted counts of card groups
-- e.g., [3, 1, 1] -> ThreeOfAKind, [2, 2, 1] -> TwoPair
getHandTypeFromCounts :: [Int] -> HandType
getHandTypeFromCounts counts = case counts of
    [5]         -> FiveOfAKind
    [4, 1]      -> FourOfAKind
    [3, 2]      -> FullHouse
    [3, 1, 1]   -> ThreeOfAKind
    [2, 2, 1]   -> TwoPair
    [2, 1, 1, 1]-> OnePair
    [1, 1, 1, 1, 1] -> HighCard
    _           -> error $ "Invalid counts pattern: " ++ show counts -- Should not happen for 5 cards

-- Calculate hand type for Part 1 (no Jokers)
getHandTypePart1 :: String -> HandType
getHandTypePart1 hand =
    let counts = sortOn Down $ map length $ group $ sort hand
    in getHandTypeFromCounts counts

-- Calculate hand type for Part 2 (with Jokers)
getHandTypePart2 :: String -> HandType
getHandTypePart2 hand =
    let numJokers = length $ filter (== 'J') hand
        -- Count non-joker cards
        nonJokerCounts = sortOn Down $ map length $ group $ sort $ filter (/= 'J') hand
        -- Add jokers to the most frequent non-joker card count
        effectiveCounts = case nonJokerCounts of
                            []     -> [numJokers] -- All Jokers case (e.g., "JJJJJ")
                            (x:xs) -> (x + numJokers) : xs
    in if numJokers == 5 then FiveOfAKind else getHandTypeFromCounts effectiveCounts

-- Core solver function
solve :: (Char -> Int) -> (String -> HandType) -> String -> Int
solve cardValueFunc getHandTypeFunc input =
    let parsedLines = map parseLine $ lines input
        handInfos = map createHandInfo parsedLines
        sortedHandInfos = sort handInfos
        -- Calculate total winnings: sum of (rank * bid)
        totalWinnings = sum $ zipWith (\rank info -> rank * hiBid info) [1..] sortedHandInfos
    in totalWinnings
    where
        createHandInfo :: (String, Int) -> HandInfo
        createHandInfo (hand, bid) = HandInfo {
            hiHand = hand,
            hiBid = bid,
            hiType = getHandTypeFunc hand,
            hiCardValues = map cardValueFunc hand
        }

-- Main entry point
main :: IO ()
main = do
    -- Read input from the file
    contents <- readFile "input.txt"

    -- Calculate and print Part 1 result
    let cardValue1 = cardValue 11 -- J = 11 for Part 1
    let result1 = solve cardValue1 getHandTypePart1 contents
    putStrLn $ "Part 1: " ++ show result1

    -- Calculate and print Part 2 result
    let cardValue2 = cardValue 1  -- J = 1 for Part 2
    let result2 = solve cardValue2 getHandTypePart2 contents
    putStrLn $ "Part 2: " ++ show result2
