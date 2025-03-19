
import qualified Data.Map as M

main :: IO ()
main = do
  target <- read <$> readFile "input.txt"
  let grid = M.fromList [((0, 0), 1)]
  let result = solve target grid (0, 0) (0, -1)
  print result

solve :: Int -> M.Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Int
solve target grid (x, y) (dx, dy)
  | value > target = value
  | otherwise = solve target (M.insert (x', y') value grid) (x', y') (dx', dy')
  where
    (dx', dy') = if x == y || (x < 0 && x == -y) || (x > 0 && x == 1 - y)
                  then (-dy, dx)
                  else (dx, dy)
    x' = x + dx'
    y' = y + dy'
    value = sum [M.findWithDefault 0 (x' + i, y' + j) grid | i <- [-1..1], j <- [-1..1]]
