
import System.IO
import Data.List
import Data.Char

parseInt :: String -> Int
parseInt = read

parseLevels :: String -> [Int]
parseLevels = map parseInt . words

isSafeReport :: [Int] -> Bool
isSafeReport levels =
  case levels of
    [] -> False
    [_] -> False
    (x:y:xs) ->
      let firstDiff = y - x
          isIncreasing = firstDiff > 0
      in
        if firstDiff == 0
          then False
          else all (checkDiff isIncreasing) (zip levels (tail levels))
  where
    checkDiff :: Bool -> (Int, Int) -> Bool
    checkDiff isIncreasing (a, b) =
      let diff = b - a
          absDiff = abs diff
      in
        diff /= 0 &&
        ((isIncreasing && diff > 0) || (not isIncreasing && diff < 0)) &&
        absDiff >= 1 && absDiff <= 3

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let linesOfFile = lines contents
  let levelsList = map parseLevels linesOfFile
  let safeCount = length $ filter isSafeReport levelsList
  print safeCount
