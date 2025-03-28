
import Data.List (sort, group, sortOn)
import Data.Ord (comparing, Down(..))
import System.IO (readFile)
import Control.Applicative ((<|>)) -- For potential error handling, though not strictly needed here

-- Define the possible card labels and their strengths
data Card = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | CT | CJ | CQ | CK | CA
    deriving (Eq, Ord, Enum, Bounded, Show)

-- Function to parse a character into a Card
parseCard :: Char -> Maybe Card
parseCard '2' = Just C2
parseCard '3' = Just C3
parseCard '4' = Just C4
parseCard '5' = Just C5
parseCard '6' = Just C6
parseCard '7' = Just C7
parseCard '8' = Just C8
parseCard '9' = Just C9
parseCard 'T' = Just CT
parseCard 'J' = Just CJ
parseCard 'Q' = Just CQ
parseCard 'K' = Just CK
parseCard 'A' = Just CA
parseCard _   = Nothing -- Handle invalid characters if necessary

-- Define the types of hands, ordered from weakest to strongest
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
    deriving (Eq, Ord, Enum, Bounded, Show)

-- Represents a hand with its original string, parsed cards, type, and bid
data HandInfo = HandInfo {
    hiCards :: [Card],     -- Parsed cards for tie-breaking comparison
    hiType  :: HandType,   -- The determined type of the hand
    hiBid   :: Int         -- The bid associated with the hand
} deriving (Show)

-- Define equality based on type and then card sequence
instance Eq HandInfo where
    (HandInfo c1 t1 _) == (HandInfo c2 t2 _) = t1 == t2 && c1 == c2

-- Define ordering based first on type, then on card sequence (lexicographically)
instance Ord HandInfo where
    compare h1 h2 =
        case compare (hiType h1) (hiType h2) of
            EQ    -> compare (hiCards h1) (hiCards h2) -- Tie-breaker: compare cards
            other -> other                             -- Primary sort: by HandType

-- Function to determine the HandType from a list of Cards
determineHandType :: [Card] -> HandType
determineHandType cards =
    let counts = map length $ group $ sort cards -- Counts of each card rank
        sortedCountsDesc = sortOn Down counts     -- Sort counts descendingly
    in case sortedCountsDesc of
        [5]             -> FiveOfAKind
        [4, 1]          -> FourOfAKind
        [3, 2]          -> FullHouse
        [3, 1, 1]       -> ThreeOfAKind
        [2, 2, 1]       -> TwoPair
        [2, 1, 1, 1]    -> OnePair
        [1, 1, 1, 1, 1] -> HighCard
        _               -> error $ "Invalid hand pattern: " ++ show counts -- Should not happen with 5 cards

-- Function to parse a line (e.g., "32T3K 765") into a HandInfo
parseLine :: String -> Maybe HandInfo
parseLine line =
    case words line of
        [handStr, bidStr] | length handStr == 5 -> do -- Ensure hand is 5 cards
            cards <- mapM parseCard handStr          -- Parse all cards, fails if any char is invalid
            let handType = determineHandType cards
            let bid = read bidStr                    -- Assumes bid is valid integer
            Just $ HandInfo cards handType bid
        _ -> Nothing -- Handle malformed lines

-- Calculates the total winnings based on the sorted list of HandInfo
calculateTotalWinnings :: [HandInfo] -> Int
calculateTotalWinnings handInfos =
    sum $ zipWith (\rank handInfo -> rank * hiBid handInfo) [1..] (sort handInfos)
    -- 1. Sort hands from weakest to strongest (using Ord instance)
    -- 2. Assign ranks (1 for weakest, N for strongest) using zipWith [1..]
    -- 3. Multiply each hand's bid by its rank
    -- 4. Sum the results

-- Main entry point
main :: IO ()
main = do
    -- Read content from input.txt
    content <- readFile "input.txt"
    let inputLines = lines content

    -- Parse each line into HandInfo, filtering out any parsing errors
    let maybeHandInfos = map parseLine inputLines
    -- A more robust way would handle Nothing cases, but for AoC we assume valid input
    let handInfos = [hi | Just hi <- maybeHandInfos]

    -- Ensure we parsed the expected number of hands (optional check)
    -- if length handInfos /= length inputLines then
    --     putStrLn "Warning: Some lines failed to parse."
    -- else
    --     return ()

    -- Calculate and print the total winnings
    let totalWinnings = calculateTotalWinnings handInfos
    print totalWinnings

