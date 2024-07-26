
import Data.List.Split (splitOn)
import Data.List (foldl', elemIndex)
import Data.Maybe (fromJust)
import System.IO (readFile)

data OP = OP { a :: Char, b :: Char, action :: Char, name :: String, matchCount :: [Int] }

main :: IO ()
main = do
    input <- readFile "input.txt"
    let lines' = filter (not . null) (lines input)
        (sum, orderedOpCodes) = processLines lines' 0 [] []
        result = runInstructions (drop (length lines' - 2) lines') orderedOpCodes
    print (head result)

processLines :: [String] -> Int -> [OP] -> [Int] -> (Int, [(Int, OP)])
processLines [] sum opcodes ordered = (sum, ordered)
processLines (x:xs) sum opcodes ordered
    | head x == 'B' = let
        registers = map read (tail (splitOn " " x))
        instruction = map read (tail (splitOn " " (head xs)))
        result = map read (tail (splitOn " " (xs !! 1)))
        tempSum = testCode registers result instruction opcodes
        newSum = sum + if tempSum >= 3 then 1 else 0
        in processLines (drop 3 xs) newSum opcodes ordered
    | otherwise = (sum, ordered)

testCode :: [Int] -> [Int] -> [Int] -> [OP] -> Int
testCode registers result instruction opcodes = length [() | op <- opcodes, match result (runOp op registers instruction)]

match :: [Int] -> [Int] -> Bool
match r c = r == c

runOp :: OP -> [Int] -> [Int] -> [Int]
runOp (OP a b action _ _) registers instruction = foldl' update registers [0..3]
  where
    update regs i = case i of
        3 -> case action of
            '+' -> setReg (getA a) (getB b) (+) regs
            '*' -> setReg (getA a) (getB b) (*) regs
            '&' -> setReg (getA a) (getB b) (&) regs
            '|' -> setReg (getA a) (getB b) (|) regs
            'a' -> setReg (getA a) (0) const regs
            '>' -> setReg (0) (getB b) (\_ b' -> if getA a > b' then 1 else 0) regs
            '=' -> setReg (0) (getB b) (\_ b' -> if getA a == b' then 1 else 0) regs
        _ -> regs
    getA 'r' = regs !! (instruction !! 1)
    getA _   = instruction !! 1
    getB 'r' = regs !! (instruction !! 2)
    getB _   = instruction !! 2
    setReg a b f r = let r' = r ++ [0] in take (instruction !! 3) r' ++ [f a b] ++ drop (instruction !! 3 + 1) r'

runInstructions :: [String] -> [(Int, OP)] -> [Int]
runInstructions lines' orderedOpCodes = foldl' execute (replicate 4 0) instructions
  where
    instructions = map (map read . splitOn " ") lines'
    execute regs instruction = let op = fromJust (lookup (instruction !! 0) orderedOpCodes) in runOp op regs instruction

orderedOpCodes :: [OP]
orderedOpCodes = [OP 'r' 'r' '+' "addr" [], OP 'r' 'v' '+' "addi" [], OP 'r' 'r' '*' "mulr" [], OP 'r' 'v' '*' "muli" [],
                  OP 'r' 'r' '&' "banr" [], OP 'r' 'v' '&' "bani" [], OP 'r' 'r' '|' "borr" [], OP 'r' 'v' '|' "bori" [],
                  OP 'r' 'r' 'a' "setr" [], OP 'v' 'r' 'a' "seti" [], OP 'v' 'r' '>' "gtir" [], OP 'r' 'v' '>' "gtri" [],
                  OP 'r' 'r' '>' "gtrr" [], OP 'v' 'r' '=' "eqir" [], OP 'r' 'v' '=' "eqri" [], OP 'r' 'r' '=' "eqrr" []]
