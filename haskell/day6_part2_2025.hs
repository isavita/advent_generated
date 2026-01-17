
import Data.Char (isDigit, isSpace)
import Data.List (transpose, groupBy, foldl')

main :: IO ()
main = do
    txt <- readFile "input.txt"
    let lns = lines txt
    if null lns then putStrLn "Grand total: 0"
    else do
        let maxW = maximum $ map length lns
            cols = transpose [r ++ replicate (maxW - length r) ' ' | r <- lns]
            colSeps = [(all isSpace c, c) | c <- cols]
            blocks = [map snd g | g <- groupBy (\a b -> fst a == fst b) colSeps, not (fst (head g))]
            solve b = let (op, nums) = foldl' step ('+', []) b
                          step (o, n) c = (if null os then o else last os, if null ds then n else (read ds :: Integer) : n)
                            where ds = filter isDigit c
                                  os = filter (`elem` "+*") c
                      in if null nums then 0 else if op == '*' then product nums else sum nums
        putStrLn $ "Grand total: " ++ show (sum $ map solve blocks)
