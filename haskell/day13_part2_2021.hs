
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (foldl', partition, break)
import Data.Maybe (mapMaybe)

-- Data types
type Point = (Int, Int)
data Fold = FoldX Int | FoldY Int deriving (Show, Eq)

-- --- Parsing ---

-- Parses a single line like "x,y" into a Point.
parsePoint :: String -> Maybe Point
parsePoint s = case break (== ',') s of
    (xStr, ',':yStr) -> Just (read xStr, read yStr)
    _                -> Nothing -- Invalid format

-- Parses a single line like "fold along axis=value" into a Fold instruction.
parseFold :: String -> Maybe Fold
parseFold s
    | take 11 s == "fold along " = parseFoldInstruction (drop 11 s)
    | otherwise                  = Nothing -- Not a fold instruction
    where
        parseFoldInstruction instruction = case break (== '=') instruction of
            ("x", '=':valStr) -> Just $ FoldX (read valStr)
            ("y", '=':valStr) -> Just $ FoldY (read valStr)
            _                 -> Nothing -- Invalid fold format

-- Parses the entire input string into initial dots and fold instructions.
parseInput :: String -> (Set Point, [Fold])
parseInput input = (dots, folds)
  where
    (dotLines, foldLinesWithBlank) = break (== "") (lines input)
    foldLines = drop 1 foldLinesWithBlank -- Remove the blank line itself

    dots = Set.fromList $ mapMaybe parsePoint dotLines
    folds = mapMaybe parseFold foldLines

-- --- Folding Logic ---

-- Applies a single fold transformation to a point.
transformPoint :: Fold -> Point -> Point
transformPoint (FoldY k) (x, y)
    | y > k     = (x, 2 * k - y) -- Fold point up
    | otherwise = (x, y)         -- Point is above or on the fold line (though problem says never on)
transformPoint (FoldX k) (x, y)
    | x > k     = (2 * k - x, y) -- Fold point left
    | otherwise = (x, y)         -- Point is to the left or on the fold line

-- Applies a single fold instruction to the entire set of points.
-- Set.map automatically handles merging of overlapping points.
applyFold :: Fold -> Set Point -> Set Point
applyFold fold = Set.map (transformPoint fold)

-- --- Display Logic ---

-- Creates a string representation of the grid defined by the set of points.
displayGrid :: Set Point -> String
displayGrid points
    | Set.null points = ""
    | otherwise = unlines $ [ [ if Set.member (x, y) points then '#' else ' '
                              | x <- [minX..maxX] ]
                            | y <- [minY..maxY] ]
    where
        -- Find the bounding box (adjusting for potentially empty sets)
        coords = Set.toList points
        allX = map fst coords
        allY = map snd coords
        minX = minimum allX
        maxX = maximum allX
        minY = minimum allY
        maxY = maximum allY

-- --- Main Execution ---

main :: IO ()
main = do
    -- Read input from the file
    input <- readFile "input.txt"

    -- Parse the input
    let (initialDots, folds) = parseInput input

    -- --- Part 1 ---
    -- Apply only the first fold
    let firstFold = head folds
    let dotsAfterFirstFold = applyFold firstFold initialDots
    putStrLn $ "Part 1: " ++ show (Set.size dotsAfterFirstFold)

    -- --- Part 2 ---
    -- Apply all folds sequentially using foldl' for efficiency
    let finalDots = foldl' (flip applyFold) initialDots folds

    -- Print the resulting grid
    putStrLn "Part 2:"
    putStr $ displayGrid finalDots -- Use putStr instead of putStrLn to avoid extra newline
