
import qualified Data.Set as Set
import System.IO (readFile)

type Point = (Int, Int)

main :: IO ()
main = do
    txt <- readFile "input.txt"
    let (seen, dup) = foldl step (Set.empty, Set.empty) (lines txt)
    print (Set.size dup)

step :: (Set.Set Point, Set.Set Point) -> String -> (Set.Set Point, Set.Set Point)
step (seen, dup) line =
    foldl add (seen, dup) pts
  where
    ws = words line                     -- ["x1,y1","->","x2,y2"]
    (x1,y1) = parseCoord (ws !! 0)
    (x2,y2) = parseCoord (ws !! 2)

    pts | x1 == x2 = [(x1, y) | y <- range y1 y2]
        | y1 == y2 = [(x, y1) | x <- range x1 x2]
        | otherwise = []

    range a b = let lo = min a b; hi = max a b in [lo..hi]

    add (s,d) p
        | p `Set.member` s = if p `Set.member` d then (s,d) else (s, Set.insert p d)
        | otherwise        = (Set.insert p s, d)

parseCoord :: String -> Point
parseCoord s = (read xs, read ys)
  where
    (xs, _:ys) = break (== ',') s
