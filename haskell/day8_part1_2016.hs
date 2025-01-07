
import Data.List (transpose)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

type Screen = [[Bool]]

parseInstruction :: String -> Maybe (String, [Int])
parseInstruction line
  | "rect" `isPrefixOf` line =
      let parts = words line
          dims = map read $ splitBy 'x' (parts !! 1)
      in if length dims == 2 then Just ("rect", dims) else Nothing
  | "rotate row" `isPrefixOf` line =
      let parts = words line
          a = read $ filter isDigit (parts !! 2)
          b = read $ parts !! 4
      in Just ("rotate row", [a, b])
  | "rotate column" `isPrefixOf` line =
      let parts = words line
          a = read $ filter isDigit (parts !! 2)
          b = read $ parts !! 4
      in Just ("rotate column", [a, b])
  | otherwise = Nothing

splitBy :: Char -> String -> [String]
splitBy delimiter str = case break (== delimiter) str of
    (prefix, "") -> [prefix]
    (prefix, _ : suffix) -> prefix : splitBy delimiter suffix

isPrefixOf :: String -> String -> Bool
isPrefixOf prefix str = take (length prefix) str == prefix

applyInstruction :: Screen -> (String, [Int]) -> Screen
applyInstruction screen ("rect", [a, b]) =
  let rows = length screen
      cols = length (head screen)
  in  [ [ if x < a && y < b then True else screen !! y !! x | x <- [0..cols-1] ] | y <- [0..rows-1] ]
applyInstruction screen ("rotate row", [y, b]) =
  let row = screen !! y
      rotatedRow = drop (length row - b `mod` length row) row ++ take (length row - b `mod` length row) row
  in  [ if i == y then rotatedRow else screen !! i | i <- [0..length screen - 1] ]
applyInstruction screen ("rotate column", [x, b]) =
  let cols = transpose screen
      col = cols !! x
      rotatedCol = drop (length col - b `mod` length col) col ++ take (length col - b `mod` length col) col
  in  transpose [ if i == x then rotatedCol else cols !! i | i <- [0..length cols - 1] ]
applyInstruction screen _ = screen

countLitPixels :: Screen -> Int
countLitPixels screen = sum $ map (length . filter id) screen

initialScreen :: Int -> Int -> Screen
initialScreen rows cols = replicate rows (replicate cols False)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let instructions = map (fromMaybe ("invalid", [] ) . parseInstruction) (lines contents)
  let finalScreen = foldl applyInstruction (initialScreen 6 50) instructions
  print $ countLitPixels finalScreen
