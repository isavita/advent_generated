
import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf)

digits :: [String]
digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

findFirstAndLastDigit :: String -> (Int, Int)
findFirstAndLastDigit line = (first, last)
  where
    (first, last, _) = foldl (\(f, l, i) c ->
      let
        updateDigit j = (if f == 0 then j else f, j, i + 1)
      in
        if isDigit c then
          updateDigit (digitToInt c)
        else
          case foldl (\acc (j, digit) -> if isPrefixOf digit (drop i line) then Just j else acc) Nothing (zip [0..] digits) of
            Just j -> updateDigit j
            Nothing -> (f, l, i + 1)
      ) (0, 0, 0) line

main :: IO ()
main = do
  content <- readFile "input.txt"
  let lns = lines content
  let result = sum $ map (\line -> let (first, last) = findFirstAndLastDigit line in 10 * first + last) lns
  print result
