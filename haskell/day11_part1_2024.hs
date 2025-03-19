
import Data.Char (isDigit)
import Data.List (dropWhile)

evenDigits :: String -> Bool
evenDigits s = even (length s)

trimLeadingZeros :: String -> String
trimLeadingZeros s = if all (== '0') s then "0" else dropWhile (== '0') s

processStones :: [String] -> [String]
processStones stones = concatMap processSingle stones
  where
    processSingle "0" = ["1"]
    processSingle s
      | evenDigits s =
        let mid = length s `div` 2
            left = trimLeadingZeros (take mid s)
            right = trimLeadingZeros (drop mid s)
         in [left, right]
      | otherwise = [show (read s * 2024 :: Integer)]

main :: IO ()
main = do
  line <- readFile "input.txt"
  let initialStones = words (filter (/= '\n') line)
  let finalStones = iterate processStones initialStones !! 25
  print (length finalStones)
