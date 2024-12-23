
import Data.Char (isDigit)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Data.Ord (comparing)

type Point = (Int, Int)
type Grid = [[Char]]
type Graph = M.Map Int (M.Map Int Int)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let grid = lines input
  let graph = buildGraph grid
  print $ solve graph 0 M.empty True

buildGraph :: Grid -> Graph
buildGraph grid = M.fromList $ map (\(p, c) -> (digitToInt c, bfs grid p)) poiCoords
  where
    poiCoords = [( (r, c), cell) | (r, row) <- zip [0..] grid, (c, cell) <- zip [0..] row, isDigit cell]

bfs :: Grid -> Point -> M.Map Int Int
bfs grid start = go M.empty [Node start 0] M.empty
  where
    rows = length grid
    cols = length (head grid)
    isValid (r, c) = r >= 0 && r < rows && c >= 0 && c < cols && grid !! r !! c /= '#'
    dirs = [(0, -1), (0, 1), (1, 0), (-1, 0)]

    go poiDistances [] _ = poiDistances
    go poiDistances (Node (r, c) dist : q) visited
      | M.member (r, c) visited = go poiDistances q visited
      | otherwise =
        let newVisited = M.insert (r, c) True visited
            newPoiDistances =
              if isDigit (grid !! r !! c)
                then M.insert (digitToInt (grid !! r !! c)) dist poiDistances
                else poiDistances
            nextNodes =
              [ Node (nr, nc) (dist + 1)
              | (dr, dc) <- dirs
              , let nr = r + dr
              , let nc = c + dc
              , isValid (nr, nc)
              ]
         in go newPoiDistances (q ++ nextNodes) newVisited

data Node = Node
  { point :: Point,
    distance :: Int
  }
  deriving (Show, Eq, Ord)

solve :: Graph -> Int -> M.Map Int Bool -> Bool -> Int
solve graph entry visited returnToZero
  | M.size visited == M.size graph = if returnToZero then fromMaybe maxBound (M.lookup 0 =<< M.lookup entry graph) else 0
  | otherwise =
    let neighbors = fromMaybe M.empty (M.lookup entry graph)
        unvisited = M.filterWithKey (\k _ -> not (M.member k visited)) neighbors
     in minimum
          [ dist + solve graph next (M.insert next True visited) returnToZero
          | (next, dist) <- M.toList unvisited
          ]

digitToInt :: Char -> Int
digitToInt c = fromEnum c - fromEnum '0'
