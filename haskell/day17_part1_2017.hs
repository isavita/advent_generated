
import System.IO

main = do
    contents <- readFile "input.txt"
    let steps = read (head (lines contents)) :: Int
    let result = solveProblem steps
    print result

solveProblem :: Int -> Int
solveProblem steps = go [0] 0 1
    where
        go buffer currentPos i
            | i > 2017 = buffer !! ((pos2017 + 1) `mod` length buffer)
            | otherwise = go newBuffer newPos (i + 1)
            where
                newPos = ((currentPos + steps) `mod` length buffer) + 1
                (before, after) = splitAt newPos buffer
                newBuffer = before ++ [i] ++ after
                pos2017 = head [j | (j, val) <- zip [0..] buffer, val == 2017]
