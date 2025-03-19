
module Main where

import Data.Char (isSpace)
import qualified Data.Set as S

type Point = (Int, Int)
type Grid = [[Char]]
data Direction = U | R | D | L deriving (Eq, Show, Enum)

nextDir :: Direction -> Direction
nextDir L = U
nextDir d = toEnum (fromEnum (d) + 1)

move :: Point -> Direction -> Point
move (x, y) U = (x, y - 1)
move (x, y) R = (x + 1, y)
move (x, y) D = (x, y + 1)
move (x, y) L = (x - 1, y)

isValid :: Grid -> Point -> Bool
isValid grid (x, y) =
  let h = length grid
      w = if h > 0 then length (head grid) else 0
  in x >= 0 && x < w && y >= 0 && y < h

solve :: Grid -> Point -> Direction -> S.Set Point -> Int
solve grid startPos startDir visited =
  go startPos startDir visited
  where
    go pos dir visited' =
      let nextPos = move pos dir
      in if not (isValid grid nextPos) then S.size visited'
         else case (grid !! snd nextPos) !! fst nextPos of
                '#' -> go pos (nextDir dir) visited'
                _   -> go nextPos dir (S.insert nextPos visited')

findStart :: Grid -> (Point, Direction)
findStart grid =
  let h = length grid
      w = if h > 0 then length (head grid) else 0
      findStart' :: Int -> Int -> Maybe (Point, Direction)
      findStart' x y
        | y >= h = Nothing
        | x >= w = findStart' 0 (y + 1)
        | (grid !! y) !! x == '^' = Just ((x, y), U)
        | otherwise = findStart' (x + 1) y
  in case findStart' 0 0 of
       Just (p, d) -> (p,d)
       Nothing -> error "No start"

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let grid = lines inputStr
  let (startPos, startDir) = findStart grid
  let result = solve grid startPos startDir (S.singleton startPos)
  print result
