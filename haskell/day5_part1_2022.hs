
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Foldable as F
import Data.List (transpose, findIndex, break)
import Data.Maybe (mapMaybe, fromJust, isJust)
import Data.Char (isUpper, isDigit)
import Control.Monad (foldM)
import System.IO (readFile)

-- Type aliases for clarity
type Crate = Char
type Stack = [Crate] -- Represents a stack, head is the top
type Stacks = Seq Stack -- Use Sequence for efficient updates
data Move = Move { count :: Int, source :: Int, destination :: Int } deriving Show

-- Function to parse the initial stack configuration
-- Takes lines representing the stack drawing (excluding the number line)
-- and the number line itself.
parseStacks :: [String] -> String -> Stacks
parseStacks drawingLines numberLine =
    let
        -- Find the character index for each stack number (1-based)
        -- Example: " 1   2   3 " -> indices are 1, 5, 9
        stackIndices = map fst $ filter (isDigit . snd) $ zip [0..] numberLine
        numStacks = length stackIndices

        -- Extract crate characters column by column based on stackIndices
        columns = map (\idx -> mapMaybe (\line -> safeGet line idx) drawingLines) stackIndices

        -- Filter out spaces and create stacks (bottom-up, so head is top)
        stacks = map (filter isUpper) columns

    in Seq.fromList stacks

-- Helper to safely get a character from a string index
safeGet :: String -> Int -> Maybe Char
safeGet s i
    | i >= 0 && i < length s = Just (s !! i)
    | otherwise              = Nothing

-- Function to parse a single move instruction line
-- Example: "move 3 from 1 to 3" -> Move {count=3, source=0, destination=2} (0-based index)
parseMove :: String -> Maybe Move
parseMove line =
    case words line of
        ["move", n, "from", s, "to", d] ->
            Just Move { count = read n
                      , source = read s - 1 -- Convert to 0-based index
                      , destination = read d - 1 -- Convert to 0-based index
                      }
        _ -> Nothing -- Ignore invalid lines

-- Function to apply a single move to the stacks (Part 1 logic: one by one)
applyMove :: Stacks -> Move -> Stacks
applyMove stacks (Move n srcIdx destIdx) =
    let
        (Just srcStack) = Seq.lookup srcIdx stacks
        (Just destStack) = Seq.lookup destIdx stacks

        -- Crates to move (taken from the top/head of source)
        cratesToMove = take n srcStack
        -- Remaining source stack
        newSrcStack = drop n srcStack
        -- New destination stack (add moved crates reversed, one by one)
        newDestStack = reverse cratesToMove ++ destStack

        -- Update the stacks sequence
        -- Using adjust for potentially better performance than update if Seq has optimizations
        stacks' = Seq.update srcIdx newSrcStack stacks
        stacks'' = Seq.update destIdx newDestStack stacks'
    in stacks''

-- Function to get the top crate from each stack
getTopCrates :: Stacks -> String
getTopCrates = mapMaybe (safeHead . F.toList) . F.toList -- Convert Seq to list of lists, then get heads
  where
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x:_) = Just x

-- Main function
main :: IO ()
main = do
    -- Read input file
    content <- readFile "input.txt"
    let allLines = lines content

    -- Separate stack drawing from move instructions
    let (stackLinesWithNumbers, moveLinesRaw) = break (== "") allLines
    let drawingLines = init stackLinesWithNumbers -- All lines except the last (numbers)
    let numberLine = last stackLinesWithNumbers
    let moveLines = drop 1 moveLinesRaw -- Drop the empty separator line

    -- Parse initial state
    let initialStacks = parseStacks drawingLines numberLine

    -- Parse moves
    let moves = mapMaybe parseMove moveLines

    -- Apply all moves sequentially using foldl'
    let finalStacks = foldl applyMove initialStacks moves
    -- let finalStacks = F.foldl' applyMove initialStacks moves -- Using Data.Foldable.foldl'

    -- Get the top crates
    let topCrates = getTopCrates finalStacks

    -- Print the result
    putStrLn topCrates

