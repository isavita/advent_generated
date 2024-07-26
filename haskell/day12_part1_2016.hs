
import qualified Data.Map as M
import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let instructions = lines contents
        registers = M.fromList [("a", 0), ("b", 0), ("c", 0), ("d", 0)]
        finalRegisters = executeInstructions instructions registers 0
    print $ M.findWithDefault 0 "a" finalRegisters

executeInstructions :: [String] -> M.Map String Int -> Int -> M.Map String Int
executeInstructions instructions registers i
    | i < 0 || i >= length instructions = registers
    | otherwise = case words (instructions !! i) of
        ["cpy", x, y] -> executeInstructions instructions (M.insert y (getValue x registers) registers) (i + 1)
        ["inc", x]    -> executeInstructions instructions (M.adjust (+1) x registers) (i + 1)
        ["dec", x]    -> executeInstructions instructions (M.adjust (subtract 1) x registers) (i + 1)
        ["jnz", x, y] -> let jump = getValue y registers in
                         if getValue x registers /= 0
                         then executeInstructions instructions registers (i + jump)
                         else executeInstructions instructions registers (i + 1)

getValue :: String -> M.Map String Int -> Int
getValue s registers = M.findWithDefault (read s) s registers
