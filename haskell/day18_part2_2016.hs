
import Data.List (foldl')

-- Function to determine the next row based on the rules
nextRow :: String -> String
nextRow row = map nextTile (zip3 ('.' : row) row (tail row ++ ['.']))
  where
    nextTile (left, center, right)
      | left == '^' && center == '^' && right == '.' = '^'
      | center == '^' && right == '^' && left == '.' = '^'
      | left == '^' && center == '.' && right == '.' = '^'
      | left == '.' && center == '.' && right == '^' = '^'
      | otherwise = '.'

-- Function to count safe tiles in a row
countSafe :: String -> Int
countSafe = length . filter (== '.')

-- Function to generate all rows and count safe tiles
solve :: String -> Int -> Int
solve initialRow numRows =
  sum $ map countSafe $ take numRows $ iterate nextRow initialRow

main :: IO ()
main = do
  input <- readFile "input.txt"
  let initialRow = head $ lines input
  let part1 = solve initialRow 40
  let part2 = solve initialRow 400000
  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2
