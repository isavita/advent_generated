
import System.IO (readFile)
import Data.List (foldl')
import Data.Maybe (mapMaybe)

-- | Main entry point for the program.
-- It reads from 'input.txt' and outputs the result to standard output.
main :: IO ()
main = do
    -- Efficiently read the file contents. 
    -- 'readFile' is lazy, so it won't load the whole file into memory at once 
    -- unless required by the processing logic.
    content <- readFile "input.txt"
    
    -- Parse the points into a list of (Int, Int).
    -- We filter out any empty lines or malformed input using mapMaybe.
    let points = mapMaybe parsePoint (lines content)
    
    -- Solve the challenge and print the maximum area found.
    print $ solve points

-- | Simple parser to convert a "x,y" string into a tuple of integers.
parsePoint :: String -> Maybe (Int, Int)
parsePoint s
    | null s = Nothing
    | otherwise = case break (== ',') s of
        (xStr, ',':yStr) -> Just (read xStr, read yStr)
        _                -> Nothing

-- | Computes the maximum area of a rectangle formed by any two points in the list.
-- This function uses O(N^2) time complexity, which is efficient for typical 
-- coordinate-based challenges.
solve :: [(Int, Int)] -> Int
solve points = findMax points 0
  where
    -- 'findMax' recursively traverses the list, comparing each point 'p'
    -- with every subsequent point 'p2' in the list 'ps'.
    findMax [] currentMax = currentMax
    findMax (p:ps) currentMax = 
        -- We use a strict fold 'foldl'' to keep memory usage low while calculating
        -- the maximum area for the current point 'p'.
        let maxWithP = foldl' (\m p2 -> max m (calcArea p p2)) currentMax ps
        in findMax ps maxWithP

    -- Calculates the area of the rectangle formed by two points as opposite corners.
    -- Area = (width + 1) * (height + 1)
    calcArea (x1, y1) (x2, y2) = 
        let width  = abs (x1 - x2) + 1
            height = abs (y1 - y2) + 1
        in width * height

