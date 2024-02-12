
import Data.List
import Data.Maybe
import Control.Monad

side :: Int
side = 5

square :: Int
square = side * side

parse :: String -> [Bool]
parse input = map (== '#') $ concat $ lines input

next1 :: [Bool] -> [Bool]
next1 grid = map (\i -> nextState i grid) [0..square-1]
  where
    nextState i grid = case (i `div` side, i `mod` side) of
      (row, col) -> let
        neighbours = length $ filter id $ map (\(r, c) -> grid !! (r * side + c)) $
          catMaybes [guard (row > 0) >> Just (row - 1, col),
                     guard (row < side - 1) >> Just (row + 1, col),
                     guard (col > 0) >> Just (row, col - 1),
                     guard (col < side - 1) >> Just (row, col + 1)]
        in case grid !! i of
          True -> if neighbours /= 1 then False else True
          False -> if neighbours == 1 || neighbours == 2 then True else False

biodiversity :: [Bool] -> Int
biodiversity grid = sum [2 ^ i | (i, cell) <- zip [0..] grid, cell]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let grid = parse input
  let result = findDuplicate (iterate next1 grid) []
  putStrLn $ show $ biodiversity result

findDuplicate :: Eq a => [a] -> [a] -> a
findDuplicate (x:xs) seen
  | x `elem` seen = x
  | otherwise = findDuplicate xs (x:seen)
