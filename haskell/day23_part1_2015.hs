
import System.IO
import Data.Array (Array, listArray, (!), bounds, inRange)
import Data.Maybe (fromMaybe)
import Control.Arrow ((>>>))

-- Data types for registers and instructions
data Register = A | B deriving (Eq, Show, Read)
data Instruction
    = Hlf Register
    | Tpl Register
    | Inc Register
    | Jmp Int
    | Jie Register Int
    | Jio Register Int
    deriving (Show)

-- Type alias for the computer state: (Register A value, Register B value)
type Registers = (Int, Int)

-- Type alias for the program represented as an array of instructions
type Program = Array Int Instruction

-- Represents the full state of the machine during execution
-- (Current Registers, Instruction Pointer, Program Code)
type MachineState = (Registers, Int, Program)

-- Parse a single line of input into an Instruction
parseInstruction :: String -> Instruction
parseInstruction line = case words line of
    ["hlf", r]     -> Hlf (parseRegister r)
    ["tpl", r]     -> Tpl (parseRegister r)
    ["inc", r]     -> Inc (parseRegister r)
    ["jmp", offset] -> Jmp (parseOffset offset)
    ["jie", r, offset] -> Jie (parseRegister (init r)) (parseOffset offset) -- Remove trailing comma
    ["jio", r, offset] -> Jio (parseRegister (init r)) (parseOffset offset) -- Remove trailing comma
    _                  -> error $ "Invalid instruction: " ++ line

-- Helper to parse register name
parseRegister :: String -> Register
parseRegister "a" = A
parseRegister "b" = B
parseRegister s   = error $ "Invalid register: " ++ s

-- Helper to parse jump offset (handles '+' sign)
parseOffset :: String -> Int
parseOffset ('+':s) = read s
parseOffset s       = read s -- Handles negative numbers directly

-- Parse the entire program input
parseProgram :: String -> Program
parseProgram input = listArray (0, length instructions - 1) instructions
  where
    instructions = map parseInstruction (lines input)

-- Get the value of a specific register
getReg :: Register -> Registers -> Int
getReg A (valA, _) = valA
getReg B (_, valB) = valB

-- Update the value of a specific register using a function
updateReg :: Register -> (Int -> Int) -> Registers -> Registers
updateReg A f (valA, valB) = (f valA, valB)
updateReg B f (valA, valB) = (valA, f valB)

-- Execute a single step of the program
step :: MachineState -> MachineState
step (regs, ip, prog) =
    let instruction = prog ! ip
    in case instruction of
        Hlf r -> (updateReg r (`div` 2) regs, ip + 1, prog)
        Tpl r -> (updateReg r (* 3) regs, ip + 1, prog)
        Inc r -> (updateReg r (+ 1) regs, ip + 1, prog)
        Jmp offset -> (regs, ip + offset, prog)
        Jie r offset ->
            if even (getReg r regs)
            then (regs, ip + offset, prog)
            else (regs, ip + 1, prog)
        Jio r offset ->
            if getReg r regs == 1
            then (regs, ip + offset, prog)
            else (regs, ip + 1, prog)

-- Run the program until the instruction pointer is out of bounds
run :: MachineState -> Registers
run state@(regs, ip, prog)
    | inRange (bounds prog) ip = run (step state)
    | otherwise                = regs -- Program halted, return final registers

-- Main entry point
main :: IO ()
main = do
    -- Read input file
    contents <- readFile "input.txt"

    -- Parse the program
    let program = parseProgram contents

    -- Define initial state (registers start at 0, IP starts at 0)
    let initialState = ((0, 0), 0, program)

    -- Run the simulation
    let finalRegisters = run initialState

    -- Print the final value of register b
    -- Using snd to get the second element (register B's value) from the tuple
    print (snd finalRegisters)

