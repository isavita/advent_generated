
import Data.Char (digitToInt)
import Data.List (minimumBy)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.Ord (comparing)

type Point = (Int, Int)
type Grid = M.Map Point Int

neighbors :: Point -> [Point]
neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

dijkstra :: Grid -> Point -> Point -> Int
dijkstra grid start end = dijkstra' M.empty (M.singleton start 0)
  where
    dijkstra' visited queue
      | M.null queue = -1
      | current == end = currentRisk
      | otherwise = dijkstra' (M.insert current currentRisk visited) nextQueue
      where
        (current, currentRisk) = minimumBy (comparing snd) $ M.toList queue
        nextQueue = foldl updateQueue (M.delete current queue) (filter (`M.member` grid) $ neighbors current)
        updateQueue q next =
          let newRisk = currentRisk + (fromJust $ M.lookup next grid)
          in case M.lookup next q of
            Just oldRisk | newRisk >= oldRisk -> q
            _ -> M.insert next newRisk q

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let gridLines = lines contents
      rows = length gridLines
      cols = length $ head gridLines
      grid = M.fromList $ concat $ zipWith (\r line -> zipWith (\c ch -> ((r,c), digitToInt ch)) [0..] line) [0..] gridLines
  print $ dijkstra grid (0,0) (rows-1, cols-1)
