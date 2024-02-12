
import System.IO
import Data.List

itemPriority :: Char -> Int
itemPriority item
  | item >= 'a' && item <= 'z' = fromEnum item - fromEnum 'a' + 1
  | otherwise = fromEnum item - fromEnum 'A' + 27

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let linesOfFile = lines contents
      groups = chunksOf 3 linesOfFile
      totalSum = sum $ map calculateGroupSum groups
  print totalSum

calculateGroupSum :: [String] -> Int
calculateGroupSum group = itemPriority $ head $ foldr1 intersect group

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
