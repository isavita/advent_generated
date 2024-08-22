import Data.Char
import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let totalSum = sumNumbers contents
    print totalSum

sumNumbers :: String -> Int
sumNumbers = sum . map read . words . map (\c -> if isDigit c || c == '-' then c else ' ')