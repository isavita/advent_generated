
import Data.List (tails, stripPrefix)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Data.Char (isDigit)
import Control.Monad (guard) -- For using guards within the Maybe monad

-- | Tries to parse a "mul(X,Y)" instruction at the beginning of a string.
--   X and Y must be 1-3 digit positive integers.
--   Returns Just (X*Y) if successful, Nothing otherwise.
parseMul :: String -> Maybe Int
parseMul s = do -- Using the Maybe monad for cleaner sequential parsing checks
    -- Check for "mul(" prefix
    rest1 <- stripPrefix "mul(" s

    -- Extract the first number (X)
    let (numXStr, rest2) = span isDigit rest1
    -- Validate X: must exist and have 1-3 digits
    guard (not (null numXStr) && length numXStr <= 3)
    x <- readMaybe numXStr :: Maybe Int

    -- Check for "," separator
    rest3 <- stripPrefix "," rest2

    -- Extract the second number (Y)
    let (numYStr, rest4) = span isDigit rest3
    -- Validate Y: must exist and have 1-3 digits
    guard (not (null numYStr) && length numYStr <= 3)
    y <- readMaybe numYStr :: Maybe Int

    -- Check for closing ")"
    -- We don't care what comes after the ')', just that it exists.
    _ <- stripPrefix ")" rest4

    -- If all checks passed, return the product
    return (x * y)

-- | Main entry point. Reads input.txt, parses instructions, and prints the sum.
main :: IO ()
main = do
    -- Read the entire content of the input file
    content <- readFile "input.txt"

    -- Generate all suffixes of the input string using tails.
    -- Example: tails "abc" == ["abc", "bc", "c", ""]
    let allSuffixes = tails content

    -- Attempt to parse a 'mul' instruction at the beginning of each suffix.
    -- mapMaybe applies parseMul and filters out Nothings, extracting Just values.
    let results = mapMaybe parseMul allSuffixes

    -- Calculate the sum of all successfully parsed multiplication results
    let totalSum = sum results

    -- Print the final sum to standard output
    print totalSum
