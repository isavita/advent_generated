
import qualified Data.Map as M
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.List (minimumBy)
import Data.Ord (comparing)

type Point = (Int, Int)
type Grid = M.Map Point Char
type Distances = M.Map Point Int

neighbors :: Point -> Grid -> [Point]
neighbors (x, y) grid =
  let potentialNeighbors = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
      isValidNeighbor p = case (M.lookup p grid, M.lookup (x, y) grid) of
                              (Just nextChar, Just currentChar) -> ord currentChar - ord nextChar <= 1
                              _ -> False
  in filter isValidNeighbor potentialNeighbors

djikstra :: Grid -> Point -> Distances
djikstra grid end = go M.empty [(0, end)]
  where
    go dists [] = dists
    go dists queue =
      let (d, curr) = minimumBy (comparing fst) queue
          newQueue = filter (/= (d, curr)) queue
      in if M.member curr dists
         then go dists newQueue
         else
           let newDists = M.insert curr d dists
               newNeighbors = filter (\n -> not (M.member n newDists)) (neighbors curr grid)
               updatedQueue = newQueue ++ map (\n -> (d + 1, n)) newNeighbors
           in go newDists updatedQueue

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let linesList = lines contents
      grid = M.fromList $ concatMap (\(y, line) -> map (\(x, c) -> ((x, y), c)) (zip [0..] line)) (zip [0..] linesList)
      start = fst $ head $ M.toList $ M.filter (== 'S') grid
      end = fst $ head $ M.toList $ M.filter (== 'E') grid
      grid' = M.insert start 'a' $ M.insert end 'z' grid
      dists = djikstra grid' end
      result = fromMaybe (error "Start not reachable") (M.lookup start dists)
  print result
