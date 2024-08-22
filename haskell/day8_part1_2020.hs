import System.IO
import Data.Set (Set)
import qualified Data.Set as Set

data Instruction = Acc Int | Jmp Int | Nop Int

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let instructions = map parseInstruction (lines contents)
    let (acc, _) = runProgram instructions 0 0 Set.empty
    print acc

parseInstruction :: String -> Instruction
parseInstruction line = case words line of
    ["acc", n] -> Acc (readSignedInt n)
    ["jmp", n] -> Jmp (readSignedInt n)
    ["nop", n] -> Nop (readSignedInt n)

readSignedInt :: String -> Int
readSignedInt ('+':xs) = read xs
readSignedInt xs = read xs

runProgram :: [Instruction] -> Int -> Int -> Set Int -> (Int, Bool)
runProgram instructions acc pc visited
    | pc `Set.member` visited = (acc, False)
    | pc >= length instructions = (acc, True)
    | otherwise = case instructions !! pc of
        Acc n -> runProgram instructions (acc + n) (pc + 1) (Set.insert pc visited)
        Jmp n -> runProgram instructions acc (pc + n) (Set.insert pc visited)
        Nop _ -> runProgram instructions acc (pc + 1) (Set.insert pc visited)