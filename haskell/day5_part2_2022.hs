
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (foldl', elemIndex, transpose, reverse)
import Data.Maybe (fromJust, catMaybes)
import Data.Char (isUpper, isSpace)
import System.IO (readFile, putStrLn)

-- Data Types
type Crate = Char
type Stack = [Crate] -- Head of list is the top of the stack
type Crates = Map Int Stack
type Move = (Int, Int, Int) -- (count, source, destination)

-- Parsing Logic

-- Parses a single line representing a move instruction.
-- Example: "move 3 from 1 to 3" -> (3, 1, 3)
parseMove :: String -> Move
parseMove line =
    case words line of
        ["move", countStr, "from", srcStr, "to", destStr] ->
            (read countStr, read srcStr, read destStr)
        _ -> error $ "Invalid move format: " ++ line

-- Parses the initial stack configuration.
-- Handles the drawing format and converts it into a Map Int Stack.
parseCrates :: [String] -> Crates
parseCrates drawingLines =
    let -- Remove the stack number line at the bottom
        crateLines = init drawingLines
        -- Number of stacks can be inferred from the length of the number line (trimmed)
        -- Or more robustly: find the last number mentioned.
        -- Simpler: columns are 1, 5, 9, ... => (col - 1) `div` 4 + 1 gives stack index
        numStacks = (length (head crateLines) + 1) `div` 4

        -- Function to extract crate from a specific stack index in a line
        getCrate :: Int -> String -> Maybe Crate
        getCrate stackIdx line =
            let colIndex = 1 + (stackIdx - 1) * 4
            in if colIndex < length line then
                   let char = line !! colIndex
                   in if isUpper char then Just char else Nothing
               else Nothing

        -- Get all crates for each stack index across all lines
        cratesPerStackIdx :: [[Maybe Crate]]
        cratesPerStackIdx =
            [ [ getCrate stackIdx line | line <- crateLines ]
            | stackIdx <- [1..numStacks]
            ]

        -- Filter out Nothings and convert Maybe Crate lists to actual Stacks (Strings)
        -- The parsing reads top-down, so the list needs reversing if head is top.
        -- Or, simply filter Nothings as is, which builds stack bottom-up if list head = bottom.
        -- Since we want head = top, filter Nothings directly works as needed.
        stacks :: [Stack]
        stacks = map catMaybes cratesPerStackIdx

    in Map.fromList $ zip [1..numStacks] stacks

-- Core Logic for Crate Moving

-- Applies a single move according to CrateMover 9000 rules (one by one).
applyMove9000 :: Crates -> Move -> Crates
applyMove9000 crates (count, src, dest) =
    -- Simulate moving 'count' crates one by one
    foldl' moveOne crates [1..count]
  where
    moveOne currentCrates _ =
        let srcStack = Map.findWithDefault [] src currentCrates
            destStack = Map.findWithDefault [] dest currentCrates
        in case srcStack of
             [] -> currentCrates -- Source is empty, no move possible
             (crate:restSrc) -> -- Take top crate
                 let newDestStack = crate : destStack -- Place on top of dest
                     updatedCrates = Map.insert src restSrc currentCrates
                 in Map.insert dest newDestStack updatedCrates

-- Applies a single move according to CrateMover 9001 rules (multiple at once).
applyMove9001 :: Crates -> Move -> Crates
applyMove9001 crates (count, src, dest) =
    let srcStack = Map.findWithDefault [] src crates
        destStack = Map.findWithDefault [] dest crates
        
        -- Take 'count' crates from the top of src
        cratesToMove = take count srcStack
        -- Remaining crates in src
        newSrcStack = drop count srcStack
        
        -- Add the moved crates to the top of dest, maintaining order
        newDestStack = cratesToMove ++ destStack
        
        -- Update the map
        updatedCrates = Map.insert src newSrcStack crates
    in Map.insert dest newDestStack updatedCrates

-- Helper to get the top crate of each stack. Handles empty stacks gracefully.
getTopCrates :: Crates -> String
getTopCrates crates =
    map getTop $ Map.elems $ Map.mapKeys (\k -> k) crates -- Ensure order 1..N
  where
    getTop :: Stack -> Crate
    getTop [] = ' ' -- Return space for empty stack (though problem implies non-empty)
    getTop (c:_) = c

-- Main execution logic
main :: IO ()
main = do
    -- Read input file
    contents <- readFile "input.txt"
    let inputLines = lines contents

    -- Find the blank line separating the drawing from the moves
    let blankLineIndex = fromJust $ elemIndex "" inputLines

    -- Parse the initial crate configuration
    let drawingLines = take blankLineIndex inputLines
    let initialCrates = parseCrates drawingLines

    -- Parse the move instructions
    let moveLines = drop (blankLineIndex + 1) inputLines
    let moves = map parseMove moveLines

    -- Calculate final state for Part 1 (CrateMover 9000)
    let finalCrates9000 = foldl' applyMove9000 initialCrates moves
    let topCrates9000 = getTopCrates finalCrates9000

    -- Calculate final state for Part 2 (CrateMover 9001)
    let finalCrates9001 = foldl' applyMove9001 initialCrates moves
    let topCrates9001 = getTopCrates finalCrates9001

    -- Print results
    putStrLn $ "Part 1: " ++ filter (not . isSpace) topCrates9000
    putStrLn $ "Part 2: " ++ filter (not . isSpace) topCrates9001

