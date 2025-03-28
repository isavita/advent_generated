
import Data.List (transpose, find)
import Data.Maybe (mapMaybe)
import System.IO (readFile)

-- Type Definitions
-- A Cell stores its number and whether it's marked (True) or not (False)
type Cell = (Int, Bool)
-- A Board is a 5x5 grid of Cells
type Board = [[Cell]]

-- --- Parsing Input ---

-- Parses the comma-separated list of numbers to be drawn.
parseDrawNumbers :: String -> [Int]
parseDrawNumbers s = map read $ words $ map replaceComma s
  where
    replaceComma ',' = ' '
    replaceComma c   = c

-- Parses a list of strings into a single Board.
-- Assumes 5 lines, each with 5 space-separated numbers.
parseBoard :: [String] -> Board
parseBoard lines = map parseRow lines
  where
    parseRow line = map (\n -> (read n, False)) (words line)

-- Parses the entire input string into draw numbers and a list of boards.
parseInput :: String -> Maybe ([Int], [Board])
parseInput content = case lines content of
    [] -> Nothing -- Empty input
    (drawStr:_:boardLines) -> -- Skip the blank line after draw numbers
        let nums = parseDrawNumbers drawStr
            boards = parseBoards boardLines
        in Just (nums, boards)
    _ -> Nothing -- Invalid format (less than 2 lines)
  where
    -- Recursively parses board sections (5 lines + 1 blank line)
    parseBoards :: [String] -> [Board]
    parseBoards [] = []
    parseBoards ls
      | length ls < 5 = [] -- Incomplete board at the end
      | otherwise =
          let (boardData, rest) = splitAt 5 ls
              board = parseBoard boardData
          -- Skip the blank line separator if present
          in board : parseBoards (dropWhile null rest)


-- --- Game Logic ---

-- Marks a specific number on a single board.
markNumber :: Int -> Board -> Board
markNumber num = map (map markCell)
  where
    markCell (val, marked)
      | val == num = (val, True)
      | otherwise  = (val, marked)

-- Checks if a list of Cells (row or column) is fully marked.
isLineComplete :: [Cell] -> Bool
isLineComplete = all snd -- Check if the boolean part (marked status) is True for all cells

-- Checks if a board has won (any complete row or column).
hasBoardWon :: Board -> Bool
hasBoardWon board = any isLineComplete board || any isLineComplete (transpose board)

-- Simulates drawing numbers one by one and returns the first winning board and the number that caused the win.
findWinningBoard :: [Int] -> [Board] -> Maybe (Board, Int)
findWinningBoard [] _ = Nothing -- No more numbers to draw, no winner
findWinningBoard (num:nums) boards =
    let updatedBoards = map (markNumber num) boards
    in case find hasBoardWon updatedBoards of
        Just winningBoard -> Just (winningBoard, num) -- Found a winner
        Nothing           -> findWinningBoard nums updatedBoards -- Continue with next number

-- --- Scoring ---

-- Calculates the sum of unmarked numbers on a board.
sumUnmarked :: Board -> Int
sumUnmarked board = sum $ map fst $ filter (not . snd) $ concat board

-- Calculates the final score for a winning board.
calculateScore :: Board -> Int -> Int
calculateScore board winningNum = sumUnmarked board * winningNum

-- --- Main Execution ---

main :: IO ()
main = do
    content <- readFile "input.txt"
    case parseInput content of
        Nothing -> putStrLn "Error: Could not parse input file."
        Just (drawNumbers, initialBoards) ->
            case findWinningBoard drawNumbers initialBoards of
                Nothing -> putStrLn "Error: No winning board found."
                Just (winningBoard, winningNumber) ->
                    let score = calculateScore winningBoard winningNumber
                    in print score

