
import Data.Char
import Data.Maybe
import System.IO

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lines' = filter (not . null) $ lines contents
  let sum' = sum $ mapMaybe processLine lines'
  print sum'

processLine :: String -> Maybe Int
processLine line = do
  let digits = filter isDigit line
  case digits of
    [] -> Nothing
    ds -> Just $ read [head ds, last ds]
