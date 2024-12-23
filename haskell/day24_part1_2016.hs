
import Data.Char (isDigit)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Data.Ord (comparing)

type Point = (Int, Int)
type Grid = [[Char]]
type Graph = M.Map Int (M.Map Int Int)

main :: IO ()
main = readFile "input.txt" >>= print . cleaningRobot

cleaningRobot :: String -> Int
cleaningRobot input =
  let grid = lines input
      graph = buildGraph grid
      start = 0
  in dfs graph start (M.singleton start True) False

buildGraph :: Grid -> Graph
buildGraph grid =
  let pois = findPOIs grid
      graph = M.fromList [(poi, M.empty) | poi <- M.keys pois]
  in M.mapWithKey (\poi _ -> bfsGetEdgeWeights grid pois poi) graph

findPOIs :: Grid -> M.Map Int Point
findPOIs grid =
  M.fromList [ (read [grid !! r !! c], (r, c)) | r <- [0..length grid - 1], c <- [0..length (head grid) - 1], isDigit (grid !! r !! c) ]

bfsGetEdgeWeights :: Grid -> M.Map Int Point -> Int -> M.Map Int Int
bfsGetEdgeWeights grid pois start =
  let startPos = pois M.! start
      bfs' q visited dists =
        case q of
          [] -> dists
          (p@(r, c), d) : rest ->
            if p `elem` visited
            then bfs' rest visited dists
            else
              let newVisited = p : visited
                  newDists = if isDigit (grid !! r !! c)
                              then M.insert (read [grid !! r !! c]) d dists
                              else dists
                  neighbors = [( (r + dr, c + dc), d + 1) | (dr, dc) <- [(0, -1), (0, 1), (1, 0), (-1, 0)],
                                                                let nr = r + dr
                                                                    nc = c + dc
                                                                in bounds grid (nr, nc) && grid !! nr !! nc /= '#']
              in bfs' (rest ++ neighbors) newVisited newDists
  in bfs' [(startPos, 0)] [] (M.singleton start 0)

bounds :: Grid -> Point -> Bool
bounds grid (r, c) = r >= 0 && r < length grid && c >= 0 && c < length (head grid)

dfs :: Graph -> Int -> M.Map Int Bool -> Bool -> Int
dfs graph entry visited returnToZero =
  if M.size graph == M.size visited
  then if returnToZero then fromMaybe maxBound (M.lookup 0 (graph M.! entry)) else 0
  else minimumBy (comparing id) (map (\(i, val) ->
    if not (M.member i visited)
    then val + dfs graph i (M.insert i True visited) returnToZero
    else maxBound
    ) (M.toList (graph M.! entry)))
