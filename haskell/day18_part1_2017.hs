
import System.IO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

getValue :: String -> Map String Int -> Int
getValue arg registers = case reads arg :: [(Int, String)] of
    [(val, "")] -> val
    _ -> Map.findWithDefault 0 arg registers

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let instructions = map words $ lines contents
        registers = Map.empty
        lastSound = 0

    let (_, result) = runProgram 0 instructions registers lastSound
    print result

runProgram :: Int -> [[String]] -> Map String Int -> Int -> (Int, Int)
runProgram i instructions registers lastSound
    | i < 0 || i >= length instructions = (i, 0)
    | otherwise = case instruction of
        ["snd", arg1] -> runProgram (i + 1) instructions registers (getValue arg1 registers)
        ["set", arg1, arg2] -> runProgram (i + 1) instructions (Map.insert arg1 (getValue arg2 registers) registers) lastSound
        ["add", arg1, arg2] -> runProgram (i + 1) instructions (Map.adjust (+ getValue arg2 registers) arg1 registers) lastSound
        ["mul", arg1, arg2] -> runProgram (i + 1) instructions (Map.adjust (* getValue arg2 registers) arg1 registers) lastSound
        ["mod", arg1, arg2] -> runProgram (i + 1) instructions (Map.adjust (`mod` getValue arg2 registers) arg1 registers) lastSound
        ["rcv", arg1] -> if getValue arg1 registers /= 0 then (i, lastSound) else runProgram (i + 1) instructions registers lastSound
        ["jgz", arg1, arg2] -> if getValue arg1 registers > 0 then runProgram (i + getValue arg2 registers) instructions registers lastSound else runProgram (i + 1) instructions registers lastSound
    where
        instruction = instructions !! i
