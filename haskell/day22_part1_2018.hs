
import Data.Char
import Data.List
import Data.Maybe

parseInput :: String -> (Int, (Int, Int))
parseInput s = (depth, (x, y))
  where
    ls = lines s
    depth = read $ last $ words $ head ls
    [x, y] = map read $ splitOn ',' $ last $ words $ ls !! 1

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn d s = x : splitOn d (drop 1 y)
  where
    (x, y) = break (== d) s

makeCaveSystem :: Int -> (Int, Int) -> [[Int]]
makeCaveSystem depth (tx, ty) =
  let
    geologicIndex :: Int -> Int -> Int
    geologicIndex x y
      | x == 0 && y == 0 || x == tx && y == ty = 0
      | y == 0 = x * 16807
      | x == 0 = y * 48271
      | otherwise = cave !! (y - 1) !! x * cave !! y !! (x - 1)
    cave = [[(geologicIndex x y + depth) `mod` 20183 | x <- [0..tx]] | y <- [0..ty]]
  in
    cave

calculateRiskLevel :: [[Int]] -> Int
calculateRiskLevel cave = sum $ map (`mod` 3) $ concat cave

main :: IO ()
main = do
  content <- readFile "input.txt"
  let (depth, target) = parseInput content
  let cave = makeCaveSystem depth target
  let riskLevel = calculateRiskLevel cave
  print riskLevel
