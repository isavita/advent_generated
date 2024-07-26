
import qualified Data.Map as M
import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let instructions = lines contents
        mulCount = execute instructions 0 0 M.empty
    print mulCount

execute :: [String] -> Int -> Int -> M.Map String Int -> Int
execute instructions pointer mulCount registers
    | pointer < 0 || pointer >= length instructions = mulCount
    | otherwise = let parts = words (instructions !! pointer)
                      cmd = head parts
                      x = parts !! 1
                      y = parts !! 2
                      getValue s = case reads s of
                                      [(v, "")] -> v
                                      _         -> M.findWithDefault 0 s registers
                      (newPointer, newRegisters, newMulCount) = case cmd of
                          "set" -> (pointer + 1, M.insert x (getValue y) registers, mulCount)
                          "sub" -> (pointer + 1, M.adjust (\v -> v - getValue y) x registers, mulCount)
                          "mul" -> (pointer + 1, M.adjust (\v -> v * getValue y) x registers, mulCount + 1)
                          "jnz" -> if getValue x /= 0 then (pointer + getValue y, registers, mulCount) else (pointer + 1, registers, mulCount)
                      in execute instructions newPointer newMulCount newRegisters
