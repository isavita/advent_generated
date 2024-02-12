
import System.IO

main = do
    contents <- readFile "input.txt"
    let totalScore = sum $ map calculateScore (lines contents)
    print totalScore

calculateScore :: String -> Int
calculateScore line = do
    let opponent = head line
        roundEnd = line !! 2

        yourMove = if roundEnd == 'X'
            then if opponent == 'A'
                then 'Z'
                else if opponent == 'B'
                    then 'X'
                    else 'Y'
            else if roundEnd == 'Y'
                then if opponent == 'A'
                    then 'X'
                    else if opponent == 'B'
                        then 'Y'
                        else 'Z'
            else if opponent == 'A'
                then 'Y'
                else if opponent == 'B'
                    then 'Z'
                    else 'X'

        score = case yourMove of
            'X' -> 1
            'Y' -> 2
            'Z' -> 3

    score + if (opponent == 'A' && yourMove == 'Y') || (opponent == 'B' && yourMove == 'Z') || (opponent == 'C' && yourMove == 'X')
        then 6
        else if (opponent == 'A' && yourMove == 'X') || (opponent == 'B' && yourMove == 'Y') || (opponent == 'C' && yourMove == 'Z')
            then 3
            else 0
