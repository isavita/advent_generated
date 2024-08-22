import System.IO

main = do
    contents <- readFile "input.txt"
    let linesOfFile = lines contents
        totalScore = sum $ map calculateScore linesOfFile
    print totalScore

calculateScore :: String -> Int
calculateScore line = go line [] 0
    where
        go [] _ score = score
        go (c:cs) stack score
            | c `elem` "([{<" = go cs (c:stack) score
            | otherwise = case stack of
                (s:ss) -> if isMatching s c
                          then go cs ss score
                          else score + charScore c
                [] -> score + charScore c

isMatching :: Char -> Char -> Bool
isMatching '(' ')' = True
isMatching '[' ']' = True
isMatching '{' '}' = True
isMatching '<' '>' = True
isMatching _ _ = False

charScore :: Char -> Int
charScore ')' = 3
charScore ']' = 57
charScore '}' = 1197
charScore '>' = 25137
charScore _ = 0