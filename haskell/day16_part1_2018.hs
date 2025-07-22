
import Data.Bits
import Data.List
import Text.Read
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

data Op = Op { a :: Char, b :: Char, action :: Char, name :: String } deriving (Show, Eq)

ops :: [Op]
ops = [ Op 'r' 'r' '+' "addr"
      , Op 'r' 'v' '+' "addi"
      , Op 'r' 'r' '*' "mulr"
      , Op 'r' 'v' '*' "muli"
      , Op 'r' 'r' '&' "banr"
      , Op 'r' 'v' '&' "bani"
      , Op 'r' 'r' '|' "borr"
      , Op 'r' 'v' '|' "bori"
      , Op 'r' 'r' 'a' "setr"
      , Op 'v' 'r' 'a' "seti"
      , Op 'v' 'r' '>' "gtir"
      , Op 'r' 'v' '>' "gtri"
      , Op 'r' 'r' '>' "gtrr"
      , Op 'v' 'r' '=' "eqir"
      , Op 'r' 'v' '=' "eqri"
      , Op 'r' 'r' '=' "eqrr"
      ]

runOp :: Op -> [Int] -> [Int] -> [Int]
runOp op regs [_, x, y, z] = take z regs ++ [val] ++ drop (z+1) regs
  where
    aVal = if a op == 'r' then regs !! x else x
    bVal = if b op == 'r' then regs !! y else y
    val = case action op of
            '+' -> aVal + bVal
            '*' -> aVal * bVal
            '&' -> aVal .&. bVal
            '|' -> aVal .|. bVal
            'a' -> aVal
            '>' -> if aVal > bVal then 1 else 0
            '=' -> if aVal == bVal then 1 else 0
            _   -> 0
runOp _ _ _ = error "Invalid instruction"

parseInput :: String -> [([Int], [Int], [Int])]
parseInput s = go (lines s)
  where
    go [] = []
    go (l:ls)
      | "Before:" `isPrefixOf` l = 
          let before = read $ drop 8 l
              (i:ls') = ls
              instruction = map read $ words i
              (a:ls'') = ls'
              after = read $ drop 7 a
              ls''' = dropWhile null ls''
          in (before, instruction, after) : go ls'''
      | otherwise = go (dropWhile (not . isPrefixOf "Before:") ls)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let samples = parseInput content
      count = length $ filter (\(before, inst, after) ->
        let matches = length $ filter (\op -> runOp op before inst == after) ops
        in matches >= 3) samples
  print count
