
import Data.Char
import Data.Map (Map, fromList, (!), keys)
import qualified Data.Map as M
import Data.Maybe

neighbors4 :: [(Int, Int)]
neighbors4 = [(0, 1), (0, -1), (1, 0), (-1, 0)]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let grid = fromList $ concatMap (\(y, row) -> map (\(x, c) -> ((x, y), digitToInt c)) $ zip [0..] row) $ zip [0..] $ lines input
  let maxScore = maximum $ map (calculateScore grid) $ keys grid
  print maxScore

calculateScore :: Map (Int, Int) Int -> (Int, Int) -> Int
calculateScore grid p = product $ map (viewScore grid p) neighbors4

viewScore :: Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Int
viewScore grid p n = viewScore' grid p n 0 (p `add` n)

viewScore' :: Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Int -> (Int, Int) -> Int
viewScore' grid p n view next =
  case M.lookup next grid of
    Just v -> if v >= grid ! p then view + 1 else viewScore' grid p n (view + 1) (next `add` n)
    Nothing -> view

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
