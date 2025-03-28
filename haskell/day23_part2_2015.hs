
import Data.Array (Array, listArray, bounds, (!), Ix)
import System.IO (readFile, IOMode(ReadMode), openFile, hGetContents)
import Data.List (find)
import Data.Maybe (fromJust) -- Assuming the program always terminates

-- Data Types

-- Represents the two registers 'a' and 'b'
data Register = A | B deriving (Show, Eq)

-- Represents the six possible instructions
data Instruction
    = Hlf Register     -- Halve register
    | Tpl Register     -- Triple register
    | Inc Register     -- Increment register
    | Jmp Int          -- Jump by offset
    | Jie Register Int -- Jump if even
    | Jio Register Int -- Jump if one
    deriving (Show, Eq)

-- Represents the state of the machine: registers a, b, and program counter (pc)
data State = State { regA :: Int, regB :: Int, pc :: Int } deriving (Show, Eq)

-- Parsing Functions

-- Parses a register name ("a" or "b") potentially followed by a comma
parseRegister :: String -> Register
parseRegister "a" = A
parseRegister "a," = A
parseRegister "b" = B
parseRegister "b," = B
parseRegister s = error $ "Invalid register: " ++ s

-- Parses a jump offset like "+5" or "-3"
parseOffset :: String -> Int
parseOffset ('+':s) = read s
parseOffset ('-':s) = - (read s)
parseOffset s = error $ "Invalid offset: " ++ s

-- Parses a line of input into an Instruction
parseInstruction :: String -> Instruction
parseInstruction line = case words line of
    ["hlf", r]     -> Hlf (parseRegister r)
    ["tpl", r]     -> Tpl (parseRegister r)
    ["inc", r]     -> Inc (parseRegister r)
    ["jmp", offset] -> Jmp (parseOffset offset)
    ["jie", r, offset] -> Jie (parseRegister r) (parseOffset offset)
    ["jio", r, offset] -> Jio (parseRegister r) (parseOffset offset)
    _                -> error $ "Invalid instruction line: " ++ line

-- Simulation Logic

-- Helper to get the value of a specific register from the state
getReg :: State -> Register -> Int
getReg s A = regA s
getReg s B = regB s

-- Helper to update a specific register in the state
setReg :: State -> Register -> Int -> State
setReg s A val = s { regA = val }
setReg s B val = s { regB = val }

-- Executes a single instruction and returns the next state
step :: Array Int Instruction -> State -> State
step program state =
    let currentPC   = pc state
        instruction = program ! currentPC -- Assumes pc is valid before calling step

        getRegVal r = getReg state r
        -- Applies function f to register r, returning updated state *without* pc change
        updateReg r f = setReg state r (f (getRegVal r))

    in case instruction of
        Hlf r -> (updateReg r (`div` 2)) { pc = currentPC + 1 }
        Tpl r -> (updateReg r (* 3))     { pc = currentPC + 1 }
        Inc r -> (updateReg r (+ 1))     { pc = currentPC + 1 }
        Jmp offset -> state { pc = currentPC + offset }
        Jie r offset ->
            let nextPC = if even (getRegVal r) then currentPC + offset else currentPC + 1
            in state { pc = nextPC }
        Jio r offset ->
            let nextPC = if getRegVal r == 1 then currentPC + offset else currentPC + 1
            in state { pc = nextPC }

-- Runs the simulation until the program counter is out of bounds
run :: Array Int Instruction -> State -> State
run program initialState =
    let (minIdx, maxIdx) = bounds program
        -- Check if the current state's PC is outside the valid instruction range
        isTerminated s = let currentPC = pc s in currentPC < minIdx || currentPC > maxIdx
        -- Lazily generate the sequence of states, starting from initialState
        states = iterate (step program) initialState
        -- Find the first state in the sequence where the PC is out of bounds
        finalState = find isTerminated states
    in fromJust finalState -- Assume program always terminates, so find always succeeds

-- Main Execution

main :: IO ()
main = do
    -- Read input file
    contents <- readFile "input.txt"
    let instructionList = map parseInstruction $ lines contents

    -- Convert list to an Array for efficient O(1) lookup by index
    -- Array bounds are 0 to (length - 1)
    let program = listArray (0, length instructionList - 1) instructionList

    -- Part 1: Start with a = 0, b = 0
    let initialState1 = State { regA = 0, regB = 0, pc = 0 }
    let finalState1 = run program initialState1
    putStrLn $ "Part 1: " ++ show (regB finalState1)

    -- Part 2: Start with a = 1, b = 0
    let initialState2 = State { regA = 1, regB = 0, pc = 0 }
    let finalState2 = run program initialState2
    putStrLn $ "Part 2: " ++ show (regB finalState2)
