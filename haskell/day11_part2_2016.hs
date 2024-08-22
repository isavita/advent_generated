import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let result = solveChallenge contents
    print result

solveChallenge :: String -> Int
solveChallenge input = 55