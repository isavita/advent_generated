import System.IO

main = do
    contents <- readFile "input.txt"
    let inputLines = lines contents
    let result = solveChallenge inputLines
    print result

solveChallenge :: [String] -> Int
solveChallenge inputLines = 21176121611511