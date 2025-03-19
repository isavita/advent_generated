
import Data.Char (isDigit)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let matrix = lines contents
  let result = sumOfPartNumbers matrix
  print result

sumOfPartNumbers :: [String] -> Int
sumOfPartNumbers matrix = sum $ catMaybes $ concatMap extractLineNumbers $ zip [0..] matrix
  where
    extractLineNumbers (y, row) = extractNumbers y 0 row []
    extractNumbers y x row acc
      | x >= length row = reverse acc
      | isDigit (row !! x) =
        let (numStr, rest) = span isDigit (drop x row)
            num = read numStr :: Int
            newX = x + length numStr
            hasSymbol = any (\i -> checkAdjacent matrix (x+i) y) [0..length numStr - 1]
        in extractNumbers y newX row ((if hasSymbol then Just num else Nothing) : acc)
      | otherwise = extractNumbers y (x+1) row acc

checkAdjacent :: [String] -> Int -> Int -> Bool
checkAdjacent matrix x y = any isSymbol $ catMaybes [getMatrix matrix (x+dx) (y+dy) | dy <- [-1..1], dx <- [-1..1]]

getMatrix :: [String] -> Int -> Int -> Maybe Char
getMatrix matrix x y
  | y < 0 || y >= length matrix = Nothing
  | x < 0 || x >= length (matrix !! y) = Nothing
  | otherwise = Just ((matrix !! y) !! x)

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && c /= '.'
