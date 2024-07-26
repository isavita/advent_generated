
import System.IO
import Data.List (intercalate)
import Control.Monad (forM_)

-- Function to parse the input from the file
parseInput :: String -> [(Int, Int)]
parseInput input = map parseLine (lines input)
  where
    parseLine line = let [depth, range] = map read (words $ map (\c -> if c == ':' then ' ' else c) line)
                     in (depth, range)

-- Function to calculate the severity of the trip
calculateSeverity :: [(Int, Int)] -> Int
calculateSeverity layers = sum [if caught d r then d * r else 0 | (d, r) <- layers]
  where
    caught depth range = (depth `mod` ((range - 1) * 2)) == 0

-- Main function to read from the file and compute the severity
main :: IO ()
main = do
    content <- readFile "input.txt"
    let layers = parseInput content
    let severity = calculateSeverity layers
    print severity
