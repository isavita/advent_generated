import Data.Char (isDigit)
import System.IO

modPow :: Integer -> Integer -> Integer -> Integer
modPow _ 0 m = 1 `mod` m
modPow b e m
  | even e    = let r = modPow b (e `div` 2) m in (r * r) `mod` m
  | otherwise = (b * modPow b (e - 1) m) `mod` m

main :: IO ()
main = do
  input <- readFile "input.txt"
  let nums = map read $ words $ map (\c -> if isDigit c then c else ' ') input
      row = nums !! 0
      col = nums !! 1
      diag = row + col - 1
      seqNum = (diag * (diag - 1) `div` 2) + col
      start = 20151125
      mul = 252533
      modv = 33554393
      code = (start * modPow mul (seqNum - 1) modv) `mod` modv
  print code