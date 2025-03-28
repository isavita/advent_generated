
{-# LANGUAGE TupleSections #-}

import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Data.Sequence (Seq ((:<|)), (|>))
import Data.Array (Array, (!), bounds, inRange, listArray, indices)
import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.List (foldl')
import Data.Maybe (maybe)
import System.IO (readFile)

type Grid = Array (Int, Int) Char
type Pos = (Int, Int)
type Visited = Set Pos
type SideMap = Map String (Set (Int, Int))

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let ls = filter (not . null) $ lines contents
        h = length ls
        w = if h > 0 then length (head ls) else 0
    if h == 0 || w == 0
    then print 0
    else do
        let grid = listArray ((0, 0), (h - 1, w - 1)) (concat ls)
        print $ solve grid

solve :: Grid -> Int
solve grid = fst $ foldl' processCell (0, S.empty) (indices grid)
  where
    processCell :: (Int, Visited) -> Pos -> (Int, Visited)
    processCell acc@(currentSum, globalVisited) pos =
      if S.member pos globalVisited || grid ! pos == '.'
      then acc
      else
        let target = grid ! pos
            (area, sideMap, componentVisited) = bfs grid pos target
            outer = countOuter sideMap
            newSum = currentSum + area * outer
            newGlobalVisited = S.union globalVisited componentVisited
        in (newSum, newGlobalVisited)

bfs :: Grid -> Pos -> Char -> (Int, SideMap, Visited)
bfs grid startPos target =
    let gridBounds = bounds grid
        moves = [(0, -1), (-1, 0), (0, 1), (1, 0)] -- (dr, dc) corresponding to (dy, dx) = (0,-1), (-1,0), (0,1), (1,0)

        go :: Visited -> SideMap -> Seq Pos -> (Int, SideMap, Visited)
        go visited sideMap queue =
          case queue of
            Seq.Empty -> (S.size visited, sideMap, visited)
            pos :<| q ->
                if not (inRange gridBounds pos) || S.member pos visited || grid ! pos /= target
                then go visited sideMap q
                else
                    let newVisited = S.insert pos visited
                        (nextSideMap, nextQueue) = foldl' (explore pos) (sideMap, q) moves
                    in go newVisited nextSideMap nextQueue
              where
                -- explore current_pos (side_map, queue) move -> (new_side_map, new_queue)
                explore :: Pos -> (SideMap, Seq Pos) -> (Int, Int) -> (SideMap, Seq Pos)
                explore (cr, cc) (currentSideMap, currentQueue) (dr, dc) =
                  let nr = cr + dr
                      nc = cc + dc
                      npos = (nr, nc)
                      -- Pass (dc, dr) to getLabel which expects (dx, dy) from Python code
                      label = getLabel dc dr
                  in if inRange gridBounds npos
                     then if grid ! npos == target
                          -- Add neighbor to queue only if it matches target
                          -- Visited check for this neighbor happens when it's dequeued
                          then (currentSideMap, currentQueue |> npos)
                          -- If neighbor is within bounds but not target, add to boundary
                          else (addOuter label currentSideMap nc nr, currentQueue)
                     -- If neighbor is out of bounds, add to boundary
                     else (addOuter label currentSideMap nc nr, currentQueue)

    in go S.empty M.empty (Seq.singleton startPos)

getLabel :: Int -> Int -> String
getLabel (-1) 0 = "left"
getLabel 1  0 = "right"
getLabel 0 (-1) = "up"
getLabel 0 1  = "down"
-- Added error case for robustness, though moves list should prevent this
getLabel dx dy = error $ "Invalid move delta: dx=" ++ show dx ++ ", dy=" ++ show dy

addOuter :: String -> SideMap -> Int -> Int -> SideMap
addOuter label sideMap col row =
    -- Key format matches Python: (y, x) for up/down, (x, y) for left/right
    let key = if label `elem` ["up", "down"] then (row, col) else (col, row)
    in M.alter (Just . S.insert key . maybe S.empty id) label sideMap

countOuter :: SideMap -> Int
countOuter sideMap = M.foldr (\keys acc -> acc + countOuterSide keys) 0 sideMap

countOuterSide :: Set (Int, Int) -> Int
countOuterSide keys = countSegments (S.toAscList keys) 0 Nothing

-- Counts distinct segments by checking adjacency along the second dimension of the key tuple
countSegments :: [(Int, Int)] -> Int -> Maybe (Int, Int) -> Int
countSegments [] count _ = count
countSegments (k@(i, j) : ks) count lastKey =
    let isAdjacent = case lastKey of
                       Nothing -> False
                       -- Check if the first coordinate is the same and the second differs by exactly 1
                       Just (pi, pj) -> i == pi && abs (j - pj) == 1
    in if isAdjacent
       -- If adjacent, continue the current segment
       then countSegments ks count (Just k)
       -- If not adjacent (or first element), start a new segment
       else countSegments ks (count + 1) (Just k)
