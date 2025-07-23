
import Data.Bits
import Data.List (sort)
import qualified Data.Set as S
import Data.Char (isDigit)
import Data.Maybe (catMaybes)

data Program = Program { a :: Int, b :: Int, c :: Int, program :: [Int] }

computeOperand :: Int -> Int -> Int -> Int -> Int
computeOperand val a b c = case val of
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 3
    4 -> a
    5 -> b
    6 -> c
    _ -> error "Invalid combo operand"

simulateComputer :: Program -> [Int]
simulateComputer (Program a b c program) = go a b c program 0 []
  where
    go a b c [] _ out = reverse out
    go a b c (cmd:op:rest) i out
        | cmd == 0 = go (a `shiftR` computeOperand op a b c) b c rest (i+2) out
        | cmd == 1 = go a (b `xor` op) c rest (i+2) out
        | cmd == 2 = go a (computeOperand op a b c `mod` 8) c rest (i+2) out
        | cmd == 3 = if a /= 0 then go a b c rest (op-2) out else go a b c rest (i+2) out
        | cmd == 4 = go a (b `xor` c) c rest (i+2) out
        | cmd == 5 = go a b c rest (i+2) (computeOperand op a b c `mod` 8 : out)
        | cmd == 6 = go a (a `shiftR` computeOperand op a b c) c rest (i+2) out
        | cmd == 7 = go a b (a `shiftR` computeOperand op a b c) rest (i+2) out
        | otherwise = error "Invalid opcode"
    go _ _ _ _ _ out = reverse out

check :: Program -> [Int]
check p = go [(0,0)] S.empty
  where
    target = reverse (program p)
    go [] _ = []
    go ((depth,score):stack) seen
        | S.member (depth,score) seen = go stack seen
        | depth == length target = score : go stack (S.insert (depth,score) seen)
        | otherwise = let newStack = [(d,s) | i <- [0..7],
                                              let newScore = i + 8*score,
                                              let result = simulateComputer (Program newScore (b p) (c p) (program p)),
                                              not (null result),
                                              head result == target !! depth,
                                              let d = depth + 1,
                                              let s = newScore]
                      in go (newStack ++ stack) (S.insert (depth,score) seen)

parseInput :: String -> Program
parseInput s = Program a b c program
  where
    ls = lines s
    a = read $ drop 12 $ head ls
    b = read $ drop 12 $ ls !! 1
    c = read $ drop 12 $ ls !! 2
    program = map read $ splitOn ',' $ drop 9 $ ls !! 4
    splitOn c s = case break (==c) s of
        (a, _:b) -> a : splitOn c b
        (a, _) -> [a]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let p = parseInput content
    let validValues = check p
    if null validValues
        then putStrLn "No valid values found"
        else print $ minimum validValues
