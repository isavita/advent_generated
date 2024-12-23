
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (foldl')

type Pos = (Int, Int)
type Grid = M.Map Pos Bool

step :: Grid -> Pos -> Int -> (Grid, Pos, Int, Int)
step grid (x, y) dir =
  let infected = fromMaybe False $ M.lookup (x, y) grid
      newDir = if infected then (dir + 1) `mod` 4 else (dir - 1 + 4) `mod` 4
      newGrid = if infected then M.delete (x, y) grid else M.insert (x, y) True grid
      (dx, dy) = case newDir of
        0 -> (0, -1)
        1 -> (1, 0)
        2 -> (0, 1)
        3 -> (-1, 0)
        _ -> error "Invalid direction"
      newPos = (x + dx, y + dy)
      infectedCountChange = if infected then 0 else 1
  in (newGrid, newPos, newDir, infectedCountChange)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lines' = lines contents
      startY = length lines' `div` 2
      startX = length (head lines') `div` 2
      initialGrid = M.fromList $ concat $
        zipWith (\y row ->
          zipWith (\x c -> ((x, y), c == '#')) [0..] row
        ) [0..] lines'
      (_, _, _, infectedCount) = foldl'
        (\(g, p, d, i) _ ->
          let (g', p', d', i') = step g p d
          in (g', p', d', i + i')
        ) (initialGrid, (startX, startY), 0, 0) [1..10000]
  print infectedCount
