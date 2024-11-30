
import Data.List (sort)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ranges = map parseRange (lines contents)
      sortedRanges = sort ranges
      mergedRanges = mergeRanges sortedRanges
      allowedCount = countAllowedIPs mergedRanges
  print allowedCount


parseRange :: String -> (Int, Int)
parseRange line = let [start, end] = map read (split '-' line) :: [Int] in (start, end)

split :: Char -> String -> [String]
split c s =  case dropWhile (==c) s of
                "" -> []
                s' -> let (h,t) = break (==c) s' in h : split c t

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges [] = []
mergeRanges [r] = [r]
mergeRanges (r1@(s1, e1):r2@(s2, e2):rs)
  | e1 >= s2 -1 = mergeRanges ((min s1 s2, max e1 e2):rs)
  | otherwise = r1 : mergeRanges (r2:rs)

countAllowedIPs :: [(Int, Int)] -> Int
countAllowedIPs ranges = 4294967296 - sum (map rangeSize ranges)

rangeSize :: (Int, Int) -> Int
rangeSize (start, end) = end - start + 1
