import System.IO

main = do
    contents <- readFile "input.txt"
    let program = map read $ wordsWhen (==',') contents :: [Int]
    let input = 5
    let output = runProgram program [input]
    print output

runProgram :: [Int] -> [Int] -> [Int]
runProgram program inputs = go program inputs 0 0
  where
    go prog ins ip inputIndex
        | opcode == 99 = []
        | opcode == 1 = go (writeParam 3 (param 1 + param 2)) ins (ip + 4) inputIndex
        | opcode == 2 = go (writeParam 3 (param 1 * param 2)) ins (ip + 4) inputIndex
        | opcode == 3 = go (writeParam 1 (ins !! inputIndex)) ins (ip + 2) (inputIndex + 1)
        | opcode == 4 = param 1 : go prog ins (ip + 2) inputIndex
        | opcode == 5 = go prog ins (if param 1 /= 0 then param 2 else ip + 3) inputIndex
        | opcode == 6 = go prog ins (if param 1 == 0 then param 2 else ip + 3) inputIndex
        | opcode == 7 = go (writeParam 3 (if param 1 < param 2 then 1 else 0)) ins (ip + 4) inputIndex
        | opcode == 8 = go (writeParam 3 (if param 1 == param 2 then 1 else 0)) ins (ip + 4) inputIndex
        | otherwise = error "Unknown opcode"
      where
        instruction = prog !! ip
        opcode = instruction `mod` 100
        paramMode n = (instruction `div` (10 ^ (n + 1))) `mod` 10
        param n = case paramMode n of
            0 -> prog !! (prog !! (ip + n))
            1 -> prog !! (ip + n)
            _ -> error "Unknown parameter mode"
        writeParam n value = take (prog !! (ip + n)) prog ++ [value] ++ drop ((prog !! (ip + n)) + 1) prog

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'