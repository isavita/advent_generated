
import System.IO (readFile)
import Data.List (find, foldl')
import Data.Maybe (fromMaybe)

-- Type aliases for clarity
type Range = (Int, Int, Int) -- (destination_start, source_start, length)
type Mapping = [Range]     -- A list of ranges representing one map (e.g., seed-to-soil)

-- Parses a line like "50 98 2" into a Range tuple
-- Example: parseRange "50 98 2" == (50, 98, 2)
parseRange :: String -> Range
parseRange line = case map read (words line) of
    [d, s, l] -> (d, s, l)
    _         -> error $ "Invalid range format: " ++ line

-- Applies a single mapping (list of ranges) to a number
-- If the number falls within a source range, it's mapped to the destination range.
-- Otherwise, it maps to itself.
applyMap :: Int -> Mapping -> Int
applyMap num ranges =
  -- Find the first range where the number falls within the source interval
  case find (\(_, src, len) -> num >= src && num < src + len) ranges of
    -- If found, calculate the mapped number based on the offset
    Just (dest, src, _) -> dest + (num - src)
    -- If not found in any range, the number maps to itself
    Nothing             -> num

-- Applies all mappings sequentially to a single seed number
-- Uses a strict left fold (foldl') for potentially better performance with large lists.
applyAllMaps :: Int -> [Mapping] -> Int
applyAllMaps = foldl' applyMap

-- Helper function to split a list based on a predicate.
-- Splits a list into sublists, removing the elements that satisfy the predicate.
-- Example: splitBy null ["a","b","","c","d"] == [["a","b"],["c","d"]]
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy p xs = let (first, rest) = break p xs
               in first : case rest of
                            []      -> []
                            (_:ys) -> splitBy p ys -- Skip the delimiter itself and recurse

-- Main entry point of the program
main :: IO ()
main = do
  -- Read the entire content of input.txt
  contents <- readFile "input.txt"
  -- Split the content into lines
  let ls = lines contents

  -- Parse seeds from the first line (e.g., "seeds: 79 14 55 13")
  -- 1. Take the first line: head ls
  -- 2. Split into words: words ...
  -- 3. Drop the "seeds:" label: tail ...
  -- 4. Convert remaining strings to Int: map read ...
  let seeds = map read . tail . words . head $ ls :: [Int]

  -- Parse the map sections
  -- 1. Drop the first two lines (seeds line and the following blank line): drop 2 ls
  -- 2. Split the remaining lines by blank lines (where null is true): splitBy null ...
  -- 3. For each resulting block of lines representing a map:
  --    a. Drop the header line (e.g., "seed-to-soil map:"): tail
  --    b. Parse each remaining line in the block into a Range: map parseRange ...
  -- 4. The result is a list of Mappings: [Mapping]
  let maps = map (map parseRange . tail) . splitBy null . drop 2 $ ls :: [Mapping]

  -- Calculate the final location for each initial seed by applying all maps sequentially
  let locations = map (`applyAllMaps` maps) seeds

  -- Find the minimum value among the calculated locations
  let minLocation = minimum locations

  -- Print the minimum location to standard output
  print minLocation
