
import System.IO
import Data.List
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)

-- Data structure to hold relevant node information
data Node = Node {
    nodeId :: (Int, Int), -- Using (x, y) coordinates as a unique ID
    used   :: Int,
    avail  :: Int
} deriving (Show, Eq) -- Eq is needed for the 'a /= b' check

-- Parses a single line of the input file (after headers) into a Maybe Node.
-- Returns Nothing if parsing fails, allowing graceful filtering of bad lines.
parseNode :: String -> Maybe Node
parseNode line = case words line of
    -- Example: /dev/grid/node-x0-y0   94T   73T    21T   77%
    (name:_:usedStr:availStr:_) -> do -- Use pattern matching for safety
        (x, y) <- parseCoordsFromName name
        u <- parseSize usedStr
        a <- parseSize availStr
        Just $ Node (x, y) u a
    _ -> Nothing -- Line doesn't have enough parts

-- Extracts (x, y) coordinates from the node name string.
-- Example: "/dev/grid/node-x10-y25" -> Just (10, 25)
parseCoordsFromName :: String -> Maybe (Int, Int)
parseCoordsFromName name = do
    -- Strip prefix and find 'x' coordinate part
    rest1 <- stripPrefix "/dev/grid/node-x" name
    let (xStr, rest2) = span isDigit rest1
    -- Check if xStr is non-empty and rest2 starts with '-y'
    guard (not (null xStr))
    rest3 <- stripPrefix "-y" rest2
    let (yStr, _) = span isDigit rest3
     -- Check if yStr is non-empty
    guard (not (null yStr))
    -- Read coordinates
    x <- safeRead xStr
    y <- safeRead yStr
    return (x, y)

-- Parses the size string (e.g., "73T") into an Int.
parseSize :: String -> Maybe Int
parseSize s = safeRead (filter isDigit s) -- Keep only digits and read

-- Safely read a String to an Int, returning Maybe Int
safeRead :: Read a => String -> Maybe a
safeRead s = case reads s of
    [(val, "")] -> Just val -- Ensure the whole string was consumed
    _           -> Nothing

-- Main function
main :: IO ()
main = do
    -- 1. Read input file
    contents <- readFile "input.txt"

    -- 2. Parse the relevant lines into Node data structures
    --    Skip first two header lines, then mapMaybe parseNode over the rest.
    let nodes = mapMaybe parseNode $ drop 2 $ lines contents

    -- 3. Calculate the number of viable pairs using a list comprehension
    --    A pair (a, b) is viable if:
    --      a) Node A is not empty (used a > 0)
    --      b) Nodes A and B are not the same (a /= b, using derived Eq)
    --      c) Data on A fits on B (used a <= avail b)
    let viablePairCount = length [ () -- We only need the count, not the pairs themselves
                                 | a <- nodes
                                 , b <- nodes
                                 , used a > 0      -- Condition 1: A is not empty
                                 , nodeId a /= nodeId b -- Condition 2: A and B are different nodes
                                 , used a <= avail b -- Condition 3: Data fits
                                 ]

    -- 4. Print the result to standard output
    print viablePairCount

-- Helper function for robust parsing (similar to Control.Monad.guard)
guard :: Bool -> Maybe ()
guard True  = Just ()
guard False = Nothing
