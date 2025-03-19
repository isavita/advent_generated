
import Data.Map (Map, fromList, (!?), adjust, member)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)

type Coord = (Int, Int)
type Grid = Map Coord Char

buildGrid :: [String] -> Grid
buildGrid lines = fromList [((x, y), c) | (y, row) <- zip [0..] lines, (x, c) <- zip [0..] row, c /= '.']

expand :: [String] -> Int -> [Coord]
expand lines factor =
    let
        height = length lines
        width = length (head lines)
        emptyRows = [y | (y, row) <- zip [0..] lines, all (== '.') row]
        emptyCols = [x | x <- [0..width-1], all (\row -> row !! x == '.') lines]
        expandCoord (x, y) =
          let
            dx = length (filter (< x) emptyCols) * (factor -1)
            dy = length (filter (< y) emptyRows) * (factor -1)
          in (x + dx, y + dy)
    in map expandCoord $ M.keys $ buildGrid lines

calculateLength :: Coord -> Coord -> Int
calculateLength (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

solve :: [String] -> Int
solve lines =
    let
        coords = expand lines 2
        pairs = [(c1, c2) | c1 <- coords, c2 <- coords, c1 < c2]
    in sum [calculateLength c1 c2 | (c1, c2) <- pairs]

main :: IO ()
main = do
  inputLines <- lines <$> readFile "input.txt"
  print (solve inputLines)
