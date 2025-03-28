
import Data.Array (Array, listArray, (!), (//), bounds, indices, elems)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe, listToMaybe)
import Control.Arrow (second)

-- Define the operations and instructions
data Operation = Acc | Jmp | Nop deriving (Show, Eq)
data Instruction = Instruction Operation Int deriving (Show, Eq)

-- Define the program state: (accumulator, instructionPointer, visitedPointers)
type State = (Int, Int, Set Int)

-- Define the program as an array of instructions for efficient access
type Program = Array Int Instruction

-- Parse a single line into an Instruction
parseInstruction :: String -> Maybe Instruction
parseInstruction line = case words line of
    [opStr, argStr] -> do
        op <- case opStr of
                "acc" -> Just Acc
                "jmp" -> Just Jmp
                "nop" -> Just Nop
                _     -> Nothing
        -- Handle '+' sign explicitly before reading the integer
        arg <- readMaybe (dropWhile (=='+') argStr)
        Just (Instruction op arg)
    _ -> Nothing

-- Execute a single step of the program
step :: Program -> State -> State
step program (acc, ip, visited) =
    let (Instruction op arg) = program ! ip
        visited' = Set.insert ip visited
    in case op of
        Acc -> (acc + arg, ip + 1, visited')
        Jmp -> (acc, ip + arg, visited')
        Nop -> (acc, ip + 1, visited')

-- Run the program until it loops or terminates
-- Returns Left acc if loop detected, Right acc if terminates normally
run :: Program -> State -> Either Int Int
run program state@(acc, ip, visited)
    | ip `Set.member` visited = Left acc -- Loop detected
    | ip < 0 || ip > snd (bounds program) = Right acc -- Terminated normally (ip went past the end)
    | otherwise = run program (step program state)

-- Initial state for execution
initialState :: State
initialState = (0, 0, Set.empty)

-- Solve Part 1: Run until loop and return accumulator
solvePart1 :: Program -> Int
solvePart1 program = case run program initialState of
    Left acc -> acc
    Right _  -> error "Program terminated unexpectedly in Part 1" -- Should not happen per problem description

-- Try modifying one instruction (Jmp <-> Nop) and see if it terminates
-- Returns Just accumulator if terminates, Nothing otherwise
tryModification :: Int -> Program -> Maybe Int
tryModification idx program =
    let instruction = program ! idx
    in case instruction of
        Instruction Acc _ -> Nothing -- Acc instructions are not modified
        Instruction Jmp arg ->
            let modifiedProgram = program // [(idx, Instruction Nop arg)]
            in case run modifiedProgram initialState of
                   Right acc -> Just acc
                   Left _    -> Nothing
        Instruction Nop arg ->
             -- Avoid infinite loop if Nop 0 becomes Jmp 0 at index 0
             -- Although the puzzle input might not have this edge case, it's safer.
             -- The original problem description implies Jmp 0 would still move IP.
             -- Let's stick to the rules: Jmp arg modifies IP by arg.
            let modifiedProgram = program // [(idx, Instruction Jmp arg)]
            in case run modifiedProgram initialState of
                   Right acc -> Just acc
                   Left _    -> Nothing

-- Solve Part 2: Find the single modification that makes the program terminate
solvePart2 :: Program -> Int
solvePart2 program =
    let possibleIndices = indices program
        -- Try modifying each instruction and collect successful results
        results = mapMaybe (`tryModification` program) possibleIndices
    in case listToMaybe results of
        Just acc -> acc
        Nothing  -> error "No single modification found to terminate the program"

-- Main function to read input, parse, solve, and print results
main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = mapMaybe parseInstruction $ lines input
    if null instructions
        then putStrLn "Error: Could not parse any instructions from input.txt"
        else do
            let program = listArray (0, length instructions - 1) instructions

            -- Solve and print Part 1
            let result1 = solvePart1 program
            putStrLn $ "Part 1: " ++ show result1

            -- Solve and print Part 2
            let result2 = solvePart2 program
            putStrLn $ "Part 2: " ++ show result2
