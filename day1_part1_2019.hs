
import System.IO
import Data.List

processLine :: String -> Int
processLine = read

getTotal :: [Int] -> Int
getTotal = sum . map (\m -> (m `div` 3) - 2)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let masses = map processLine (lines contents)
        total = getTotal masses
    print total
