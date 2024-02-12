
import System.IO

main = do
    contents <- readFile "input.txt"
    let counts = foldl (\acc num -> updateCounts acc num) (replicate 12 (0,0)) (lines contents)
        gammaRate = foldl (\rate i -> if (fst (counts !! i) > snd (counts !! i)) then rate + 2^(12-i-1) else rate) 0 [0..11]
        epsilonRate = foldl (\rate i -> if (fst (counts !! i) <= snd (counts !! i)) then rate + 2^(12-i-1) else rate) 0 [0..11]
    print (gammaRate * epsilonRate)

updateCounts :: [(Int, Int)] -> String -> [(Int, Int)]
updateCounts acc num = foldl (\acc' i -> updateCount acc' i (num !! i)) acc [0..11]

updateCount :: [(Int, Int)] -> Int -> Char -> [(Int, Int)]
updateCount acc i c
    | c == '0' = updateAtIndex acc i (\(x, y) -> (x + 1, y))
    | otherwise = updateAtIndex acc i (\(x, y) -> (x, y + 1))

updateAtIndex :: [a] -> Int -> (a -> a) -> [a]
updateAtIndex list idx f = take idx list ++ [f (list !! idx)] ++ drop (idx + 1) list
