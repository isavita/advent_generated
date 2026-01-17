
import Data.Char (isDigit, isSpace)

tokenize :: String -> [String]
tokenize [] = []
tokenize (c:cs)
    | isSpace c = tokenize cs
    | isDigit c = let (n, rest) = span isDigit (c:cs) in n : tokenize rest
    | otherwise = [c] : tokenize cs

solve1 :: [String] -> (Integer, [String])
solve1 toks = let (v, r) = atom toks in chain v r
  where
    chain v (op:rest) | op == "+" || op == "*" = 
        let (v2, r2) = atom rest in chain (if op == "+" then v + v2 else v * v2) r2
    chain v r = (v, r)
    atom ("(":rs) = let (v, ")":r2) = solve1 rs in (v, r2)
    atom (n:rs) = (read n, rs)

solve2 :: [String] -> (Integer, [String])
solve2 toks = let (v, r) = add2 toks in mulChain v r
  where
    mulChain v ("*":rs) = let (v2, r2) = add2 rs in mulChain (v * v2) r2
    mulChain v r = (v, r)
    add2 toks' = let (v, r) = atom2 toks' in addChain v r
    addChain v ("+":rs) = let (v2, r2) = atom2 rs in addChain (v + v2) r2
    addChain v r = (v, r)
    atom2 ("(":rs) = let (v, ")":r2) = solve2 rs in (v, r2)
    atom2 (n:rs) = (read n, rs)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let lines' = map tokenize (lines input)
    print $ sum [fst (solve1 l) | l <- lines', not (null l)]
    print $ sum [fst (solve2 l) | l <- lines', not (null l)]

