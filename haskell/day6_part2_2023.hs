
import Data.Char (isDigit)
import Data.List (dropWhile)

calculateWaysToWin :: Int -> Int -> Int
calculateWaysToWin time record = length $ filter (> record) distances
  where
    distances = map (\holdTime -> holdTime * (time - holdTime)) [1..(time -1)]

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let time     = read $ filter isDigit $ head ls :: Int
  let distance = read $ filter isDigit $ last ls :: Int
  let waysToWin = calculateWaysToWin time distance
  print waysToWin
