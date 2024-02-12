
import System.IO

main = do
    contents <- readFile "input.txt"
    let inputLines = lines contents
    let result = head [ [ c | (c, d) <- zip line1 line2, c == d ] | line1 <- inputLines, line2 <- inputLines, hammingDistance line1 line2 == 1 ]
    putStrLn result

hammingDistance :: String -> String -> Int
hammingDistance s1 s2 = length $ filter (\(c1, c2) -> c1 /= c2) $ zip s1 s2
