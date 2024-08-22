import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let program = map read $ wordsWhen (==',') contents
    runProgram program 1

runProgram :: [Int] -> Int -> IO ()
runProgram program input = go program 0
  where
    go prog i = case prog !! i `mod` 100 of
        1  -> let (a:b:c:_) = drop (i+1) prog
                  val1 = getValue prog a (getMode (prog !! i) 100)
                  val2 = getValue prog b (getMode (prog !! i) 1000)
              in go (setValue prog c (val1 + val2)) (i+4)
        2  -> let (a:b:c:_) = drop (i+1) prog
                  val1 = getValue prog a (getMode (prog !! i) 100)
                  val2 = getValue prog b (getMode (prog !! i) 1000)
              in go (setValue prog c (val1 * val2)) (i+4)
        3  -> let a = prog !! (i+1)
              in go (setValue prog a input) (i+2)
        4  -> let a = prog !! (i+1)
                  val = getValue prog a (getMode (prog !! i) 100)
              in print val >> go prog (i+2)
        99 -> return ()
        _  -> error "Unknown opcode"

getValue :: [Int] -> Int -> Int -> Int
getValue prog param mode = case mode of
    0 -> prog !! param
    1 -> param
    _ -> error "Unknown parameter mode"

setValue :: [Int] -> Int -> Int -> [Int]
setValue prog pos val = take pos prog ++ [val] ++ drop (pos+1) prog

getMode :: Int -> Int -> Int
getMode instruction divisor = (instruction `div` divisor) `mod` 10

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'