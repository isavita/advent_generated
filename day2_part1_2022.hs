
import System.IO

main = do
    contents <- readFile "input.txt"
    let totalScore = sum $ map calculateScore (lines contents)
    print totalScore

calculateScore :: String -> Int
calculateScore line = 
    let opponent = head line
        yourMove = last line
        score = case yourMove of
            'X' -> 1
            'Y' -> 2
            'Z' -> 3
            _ -> 0
    in score + case (opponent, yourMove) of
        ('A', 'Y') -> 6
        ('B', 'Z') -> 6
        ('C', 'X') -> 6
        ('A', 'X') -> 3
        ('B', 'Y') -> 3
        ('C', 'Z') -> 3
        _ -> 0
