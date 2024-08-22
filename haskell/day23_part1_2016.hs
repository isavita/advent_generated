import System.IO
import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Map as Map

data Instruction = Cpy String String | Inc String | Dec String | Jnz String String | Tgl String deriving (Show)

type Registers = Map.Map String Int

main = do
    contents <- readFile "input.txt"
    let instructions = map parseInstruction (lines contents)
    let registers = Map.fromList [("a", 7), ("b", 0), ("c", 0), ("d", 0)]
    let finalRegisters = executeInstructions instructions registers 0
    print $ Map.findWithDefault 0 "a" finalRegisters

parseInstruction :: String -> Instruction
parseInstruction line =
    case words line of
        ["cpy", x, y] -> Cpy x y
        ["inc", x] -> Inc x
        ["dec", x] -> Dec x
        ["jnz", x, y] -> Jnz x y
        ["tgl", x] -> Tgl x

executeInstructions :: [Instruction] -> Registers -> Int -> Registers
executeInstructions instructions registers pc
    | pc < 0 || pc >= length instructions = registers
    | otherwise =
        case instructions !! pc of
            Cpy x y -> executeInstructions instructions (cpy x y registers) (pc + 1)
            Inc x -> executeInstructions instructions (inc x registers) (pc + 1)
            Dec x -> executeInstructions instructions (dec x registers) (pc + 1)
            Jnz x y -> executeInstructions instructions registers (pc + jnz x y registers)
            Tgl x -> executeInstructions (tgl x pc instructions registers) registers (pc + 1)

cpy :: String -> String -> Registers -> Registers
cpy x y registers
    | Map.member y registers = Map.insert y (getValue x registers) registers
    | otherwise = registers

inc :: String -> Registers -> Registers
inc x registers = Map.insert x (Map.findWithDefault 0 x registers + 1) registers

dec :: String -> Registers -> Registers
dec x registers = Map.insert x (Map.findWithDefault 0 x registers - 1) registers

jnz :: String -> String -> Registers -> Int
jnz x y registers
    | getValue x registers /= 0 = getValue y registers
    | otherwise = 1

tgl :: String -> Int -> [Instruction] -> Registers -> [Instruction]
tgl x pc instructions registers =
    let target = pc + getValue x registers
    in if target >= 0 && target < length instructions
        then let toggled = toggle (instructions !! target)
             in take target instructions ++ [toggled] ++ drop (target + 1) instructions
        else instructions

toggle :: Instruction -> Instruction
toggle (Inc x) = Dec x
toggle (Dec x) = Inc x
toggle (Tgl x) = Inc x
toggle (Jnz x y) = Cpy x y
toggle (Cpy x y) = Jnz x y

getValue :: String -> Registers -> Int
getValue x registers = fromMaybe (read x) (Map.lookup x registers)