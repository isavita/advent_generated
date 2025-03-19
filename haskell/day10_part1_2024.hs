
module Main where

import Data.List (sort)
import qualified Data.Set as S

type Point = (Int, Int)
type Grid = [[Int]]

solve :: Grid -> Int
solve grid = sum $ map (length . floodFill) trailheads
  where
    nr = length grid
    nc = length (head grid)
    dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    trailheads = [(r, c) | r <- [0 .. nr - 1], c <- [0 .. nc - 1], grid !! r !! c == 0]

    floodFill :: Point -> S.Set Point
    floodFill th = bfs S.empty S.empty [ (th, 0) ]

    bfs :: S.Set Point -> S.Set (Int, Int, Int) -> [(Point, Int)] -> S.Set Point
    bfs reached visited [] = reached
    bfs reached visited (current@( (r,c), h ):xs )
      | h == 9 = bfs (S.insert (r,c) reached) visited xs
      | otherwise = bfs reached newVisited newQueue
      where
        neighbors = filter (\(nr, nc) -> nr >= 0 && nr < length grid && nc >= 0 && nc < length (head grid))
                    [(r + dr, c + dc) | (dr, dc) <- dirs]

        validNeighbors = [( (nr,nc) , grid !! nr !! nc) | (nr,nc) <- neighbors, grid !! nr !! nc == h + 1 ]

        newVisited = foldr (\((nr,nc),nh) acc -> S.insert (nr,nc,nh) acc ) visited validNeighbors
        next = [ (point,nh) | (point,nh) <- validNeighbors, not (S.member (fst point, snd point, nh) visited) ]
        newQueue = xs ++ next
    
main :: IO ()
main = do
  contents <- readFile "input.txt"
  let grid = map (map (read . (: []))) $ lines contents
  print $ solve grid
