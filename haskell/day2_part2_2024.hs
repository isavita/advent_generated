
import System.IO
import Control.Monad (when)

isSafe :: [Int] -> Bool
isSafe report = isSafeSequence report || any isSafeSequence (removeOne report)
  where
    removeOne xs = [take i xs ++ drop (i + 1) xs | i <- [0..length xs - 1]]

isSafeSequence :: [Int] -> Bool
isSafeSequence report
  | null report = True
  | isIncreasing || isDecreasing = all (\x -> x >= 1 && x <= 3) diffs
  | otherwise = False
  where
    isIncreasing = all (\(x, y) -> x < y) $ zip report (tail report)
    isDecreasing = all (\(x, y) -> x > y) $ zip report (tail report)
    diffs = map abs $ zipWith (-) report (tail report)

countSafeReports :: [[Int]] -> Int
countSafeReports = length . filter isSafe

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let reports = parseInput contents
  print $ countSafeReports reports
  hClose handle
