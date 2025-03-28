
{-# LANGUAGE BangPatterns #-} -- Enable strictness annotations

import System.IO (readFile)
import Data.List (sort, foldl')
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, listToMaybe)
import Text.Read (readMaybe)

-- Type Definitions
type Point = (Int, Int)
-- SensorInfo stores: Sensor Coords, Beacon Coords, Manhattan Distance between them
type SensorInfo = (Point, Point, Int)

-- Manhattan distance calculation
manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- -----------------------------------------------------------------------------
-- Parsing Input
-- -----------------------------------------------------------------------------

-- Helper to parse "x=val, y=val" format safely using readMaybe
parseCoords :: String -> Maybe Point
parseCoords str =
  case break (==',') str of
    (xPart, ',':' ':yPart) ->
      -- Expect "x=" prefix for xPart and "y=" prefix for yPart
      case (dropPrefix "x=" xPart, dropPrefix "y=" yPart) of
         (Just xStr, Just yStr) -> do -- Using Maybe monad for safe parsing
             !x <- readMaybe xStr
             !y <- readMaybe yStr
             return (x, y)
         _ -> Nothing -- Prefix missing or format incorrect
    _ -> Nothing -- Failed to find comma or format is wrong

-- Parses a full line from input.txt into SensorInfo
-- Example line: "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
parseLine :: String -> Maybe SensorInfo
parseLine s =
  -- Split the string at ": closest beacon is at "
  case breakSubstring ": closest beacon is at " s of
    (sensorPart, beaconPart) ->
      -- Process sensor part: "Sensor at x=..."
      case dropPrefix "Sensor at " sensorPart of
        Just sensorCoordsStr ->
          -- Process beacon part: "x=..."
          case parseCoords beaconPart of
            Just beacon ->
              -- Process sensor coordinates string
              case parseCoords sensorCoordsStr of
                Just sensor ->
                  -- Calculate distance and return SensorInfo
                  let !dist = manhattan sensor beacon
                  in Just (sensor, beacon, dist)
                _ -> Nothing -- Failed parsing sensor coords
            _ -> Nothing -- Failed parsing beacon coords
        _ -> Nothing -- Failed removing "Sensor at " prefix
    _ -> Nothing -- Failed splitting sensor/beacon parts

-- Helper: Find the first occurrence of a substring and split the string there.
-- Returns (part_before_needle, part_after_needle)
-- More robust than simple break if needle contains special chars (though not needed here)
breakSubstring :: String -> String -> (String, String)
breakSubstring needle haystack = go haystack []
  where
    nlen = length needle
    go :: String -> String -> (String, String) -- src, accumulator_for_prefix
    go src acc
      | take nlen src == needle = (reverse acc, drop nlen src) -- Found it
      | c:cs <- src             = go cs (c:acc)                -- Continue searching
      | otherwise               = (reverse acc, [])            -- Needle not found

-- Helper: Drop a prefix from a string if it exists
dropPrefix :: String -> String -> Maybe String
dropPrefix prefix str
  | take plen str == prefix = Just $ drop plen str
  | otherwise               = Nothing
  where plen = length prefix

-- -----------------------------------------------------------------------------
-- Part 1: Calculate excluded positions on a target row
-- -----------------------------------------------------------------------------

-- Calculate the horizontal interval [minX, maxX] covered by a single sensor's
-- exclusion zone on the target row `targetY`. Returns Nothing if the zone
-- doesn't reach the row.
getRowInterval :: Int -> SensorInfo -> Maybe (Int, Int)
getRowInterval targetY ((sx, sy), _, dist) =
    let !dy = abs (sy - targetY) -- Vertical distance from sensor to target row
        !remDist = dist - dy      -- Max horizontal distance covered at targetY
    in if remDist < 0
       then Nothing -- Sensor range doesn't reach targetY
       else Just (sx - remDist, sx + remDist)

-- Merge overlapping or adjacent intervals in a sorted list.
-- Uses a recursive helper function which is often cleaner in Haskell.
mergeIntervals :: [(Int, Int)] -> [(Int, Int)]
mergeIntervals = mergeIntervalsRecursive . sort
  where
    mergeIntervalsRecursive :: [(Int, Int)] -> [(Int, Int)]
    mergeIntervalsRecursive [] = []
    mergeIntervalsRecursive [x] = [x]
    -- Compare the first two intervals
    mergeIntervalsRecursive ((min1, max1) : (min2, max2) : rest)
        -- If they overlap or touch (min2 <= max1 + 1 allows adjacent intervals like [1,3] and [4,5] to merge)
        | min2 <= max1 + 1 =
            -- Merge them by taking the min of the first and max of the two,
            -- then recurse with the merged interval and the rest of the list.
            mergeIntervalsRecursive ((min1, max max1 max2) : rest)
        -- If they are disjoint
        | otherwise =
            -- Keep the first interval as it is, and recurse on the remaining list starting from the second interval.
            (min1, max1) : mergeIntervalsRecursive ((min2, max2) : rest)

-- Calculate the total number of integer points covered by a list of disjoint intervals.
totalLength :: [(Int, Int)] -> Int
totalLength = sum . map (\(minX, maxX) -> maxX - minX + 1)

-- Solve Part 1: Find the number of positions on targetY where a beacon cannot exist.
solvePart1 :: Int -> [SensorInfo] -> Int
solvePart1 targetY infos =
    -- 1. Calculate all horizontal coverage intervals for the target row from each sensor.
    let intervals = mapMaybe (getRowInterval targetY) infos
    -- 2. Merge these intervals into a minimal set of disjoint intervals.
        merged = mergeIntervals intervals -- Sorts and merges
    -- 3. Calculate the total number of positions covered by the merged intervals.
        !totalCov = totalLength merged
    -- 4. Find the set of unique known beacon locations that lie exactly on the target row.
        beaconsOnRow = Set.fromList [bx | (_, (bx, by), _) <- infos, by == targetY]
    -- 5. The number of positions where a beacon *cannot* exist is the total covered length
    --    minus the number of *known* beacons on that row (since a beacon *can* exist at its own location).
    --    This matches the example's logic (27 total covered - 1 known beacon = 26).
    in totalCov - Set.size beaconsOnRow

-- -----------------------------------------------------------------------------
-- Part 2: Find the distress beacon
-- -----------------------------------------------------------------------------

-- Check if a given point `p` is within the exclusion zone (Manhattan distance <= radius) of any sensor.
isCovered :: Point -> [SensorInfo] -> Bool
isCovered !p infos = any (\(s, _, d) -> manhattan p s <= d) infos

-- Solve Part 2: Find the unique uncovered point within the bounds [0, maxCoord]
-- and calculate its tuning frequency.
solvePart2 :: Int -> [SensorInfo] -> Integer -- Use Integer for result frequency due to potential size
solvePart2 maxCoord infos =
    -- The single distress beacon must lie just outside the exclusion zone of at least one sensor.
    -- This means it lies on the perimeter of a diamond with radius `d + 1` centered at some sensor `s`.
    -- The unique uncovered point is likely at an intersection of two such perimeters (from different sensors).
    -- These perimeters are defined by lines:
    -- x + y = sx + sy +/- (d + 1)
    -- x - y = sx - sy +/- (d + 1)

    -- 1. Generate the sets of constants C for all 'critical' boundary lines (radius d+1).
    --    Using Set removes duplicates automatically.
    let !criticalLinesPlus = Set.fromList $ concatMap (\((sx, sy), _, d) -> [sx + sy + d + 1, sx + sy - d - 1]) infos
        !criticalLinesMinus = Set.fromList $ concatMap (\((sx, sy), _, d) -> [sx - sy + d + 1, sx - sy - d - 1]) infos

    -- 2. Iterate through pairs of lines (one x+y=c1, one x-y=c2) using list comprehension.
    --    Find their intersection point (x, y).
        candidates = do
            c1 <- Set.toList criticalLinesPlus  -- c1 = x + y
            c2 <- Set.toList criticalLinesMinus -- c2 = x - y

            -- 3. For (x, y) to be integers, c1 and c2 must have the same parity.
            --    Check this using `even (c1 - c2)` or `even (c1 + c2)`.
            if even (c1 + c2) then do
                let !x = (c1 + c2) `div` 2
                    !y = (c1 - c2) `div` 2

                -- 4. Check if the intersection point (x, y) is within the specified bounds [0, maxCoord].
                if x >= 0 && x <= maxCoord && y >= 0 && y <= maxCoord then
                    -- 5. Crucially, check if this candidate point is NOT covered by any sensor's original exclusion zone.
                    if not (isCovered (x, y) infos) then
                        -- This point is a valid candidate for the distress beacon. Return it.
                        -- The `do` block combined with list results acts like `concatMap`.
                        return (x, y)
                    else
                        [] -- Point is covered by a sensor, discard it.
                else
                    [] -- Point is out of bounds, discard it.
            else
                [] -- Intersection is not at integer coordinates, discard it.

    -- 6. Since the problem statement implies a unique solution, take the first valid candidate found.
    in case listToMaybe candidates of
         Just (x, y) -> toInteger x * 4000000 + toInteger y -- Calculate tuning frequency using Integer
         Nothing     -> error "No solution found for Part 2 (problem guarantees one)"

-- -----------------------------------------------------------------------------
-- Main Execution
-- -----------------------------------------------------------------------------

main :: IO ()
main = do
    -- Read input file content
    contents <- readFile "input.txt"
    -- Parse lines into SensorInfo records, filtering out any lines that fail parsing.
    -- Use strict evaluation (!) on the list to parse upfront.
    let !parsedInfos = mapMaybe parseLine (lines contents)

    -- Basic check if parsing yielded any results (assuming input file is not empty)
    if null parsedInfos && not (null (lines contents))
      then putStrLn "Error: Failed to parse any valid sensor data from input.txt"
      else do
        -- --- Part 1 ---
        let targetY = 2000000 -- Target row for Part 1 as specified
        putStrLn $ "Calculating Part 1 for y=" ++ show targetY ++ "..."
        -- Calculate and print Part 1 result, use strict evaluation (!)
        let !part1Result = solvePart1 targetY parsedInfos
        putStrLn $ "Part 1: " ++ show part1Result

        -- --- Part 2 ---
        let maxCoord = 4000000 -- Coordinate bound for Part 2
        putStrLn $ "Calculating Part 2 for bounds [0, " ++ show maxCoord ++ "]..."
        -- Calculate and print Part 2 result, use strict evaluation (!)
        let !part2Result = solvePart2 maxCoord parsedInfos
        putStrLn $ "Part 2: " ++ show part2Result
