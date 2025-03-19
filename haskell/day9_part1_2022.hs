
import qualified Data.Set as Set
import Data.List (foldl')

type Point = (Int, Int)

moveHead :: Point -> String -> Point
moveHead (x, y) dir
  | dir == "R" = (x + 1, y)
  | dir == "L" = (x - 1, y)
  | dir == "U" = (x, y + 1)
  | dir == "D" = (x, y - 1)
  | otherwise  = (x, y)

moveTail :: Point -> Point -> Point
moveTail head@(hx, hy) tail@(tx, ty)
    | abs (hx - tx) > 1 || abs (hy - ty) > 1 =
        if hx /= tx && hy /= ty then
            (tx + signum (hx - tx), ty + signum (hy - ty))
        else
            (tx + signum (hx - tx), ty + signum (hy - ty))
    | otherwise = tail
  
processStep :: (Point, Point, Set.Set Point) -> String -> (Point, Point, Set.Set Point)
processStep (head, tail, visited) dir =
  let newHead = moveHead head dir
      newTail = moveTail newHead tail
  in  (newHead, newTail, Set.insert newTail visited)

processLine :: (Point, Point, Set.Set Point) -> String -> (Point, Point, Set.Set Point)
processLine (head, tail, visited) line =
    let [dir, steps] = words line
        numSteps = read steps :: Int
        moves = replicate numSteps dir
    in foldl' (\(h, t, v) d -> let newH = moveHead h d; newT = moveTail newH t in (newH, newT, Set.insert newT v)) (head,tail,visited) moves
  
main :: IO ()
main = do
  contents <- readFile "input.txt"
  let initial = ((0, 0), (0, 0), Set.singleton (0, 0))
      (_, _, visited) = foldl' processLine initial (lines contents)
  print (Set.size visited)
