
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|), (|>), index, fromList, length, drop, take) -- Explicit imports for clarity
import Data.Foldable (toList)
import Data.List (iterate, head, dropWhileEnd)
import Data.Char (isSpace)
import Data.Semigroup ((<>)) -- For Seq concatenation (works for Seq as it's a Monoid)
import System.IO (readFile)

-- Helper to trim leading/trailing whitespace from a string
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- Helper to convert a non-negative integer into a list of its digits
-- Handles single-digit and two-digit sums as per the problem description.
toDigits :: Int -> [Int]
toDigits n
  | n < 0     = error "Negative recipe score encountered, which shouldn't happen."
  | n >= 10   = [n `div` 10, n `mod` 10] -- e.g., 10 -> [1, 0]
  | otherwise = [n]                      -- e.g., 5 -> [5]

-- Represents the state of the simulation: (Recipe Scoreboard, Elf 1 Index, Elf 2 Index)
type State = (Seq Int, Int, Int)

-- Performs one step of the recipe generation process
step :: State -> State
step (recipes, elf1Pos, elf2Pos) = (newRecipeBoard, newElf1Pos, newElf2Pos)
  where
    -- 1. Get current scores
    score1 = recipes `index` elf1Pos
    score2 = recipes `index` elf2Pos
    
    -- 2. Calculate sum and get new recipe digits
    sumScores = score1 + score2
    newDigits = Seq.fromList (toDigits sumScores)
    
    -- 3. Add new recipes to the board
    newRecipeBoard = recipes <> newDigits -- Efficient append using Sequence's Monoid instance
    
    -- 4. Calculate new elf positions
    lenNew = Seq.length newRecipeBoard -- Get the length *after* adding recipes
    -- Move forward (1 + current score) steps, wrapping around using modulo
    newElf1Pos = (elf1Pos + 1 + score1) `mod` lenNew
    newElf2Pos = (elf2Pos + 1 + score2) `mod` lenNew

main :: IO ()
main = do
    -- Read the number of recipes from input.txt
    content <- readFile "input.txt"
    -- Parse the input, trimming whitespace first for robustness
    let numRecipesAfter = read (trim content) :: Int
    
    -- We need the 10 recipes *after* numRecipesAfter, so we must generate at least numRecipesAfter + 10 recipes.
    let targetLength = numRecipesAfter + 10
    
    -- Define the initial state
    let initialState :: State
        initialState = (Seq.fromList [3, 7], 0, 1) -- (Recipes [3, 7], Elf1 at index 0, Elf2 at index 1)

    -- Lazily generate the sequence of states using iterate
    -- iterate applies 'step' repeatedly, starting with 'initialState'
    let allStates = iterate step initialState

    -- Find the first state where the number of recipes is sufficient
    -- Thanks to laziness, iterate will only compute states until this condition is met.
    let (finalRecipes, _, _) = head $ filter (\(recipes, _, _) -> Seq.length recipes >= targetLength) allStates

    -- Extract the required 10 recipes:
    -- 1. Drop the first 'numRecipesAfter' recipes.
    -- 2. Take the next 10 recipes from the remaining sequence.
    let resultSeq = Seq.take 10 (Seq.drop numRecipesAfter finalRecipes)
    
    -- Convert the sequence of integer scores [5, 1, 5, ...] into a single string "515..."
    let resultString = concatMap show (toList resultSeq)
    
    -- Print the result to standard output
    putStrLn resultString
