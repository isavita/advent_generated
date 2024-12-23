
import Data.Array
import Data.Maybe
import qualified Data.Set as Set
import System.IO

type Point = (Int, Int)
type State = (Int, Int, Int)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let grid = lines input
  let h = length grid
  let w = length $ head grid
  let (startX, startY, startDir) = findStart grid
  let grid' = replace (startY, startX) '.' (listArray ((0, 0), (h - 1, w - 1)) (concat grid))
  let canLoop = countLoops grid' h w startX startY startDir
  print canLoop

findStart :: [String] -> (Int, Int, Int)
findStart grid = fromJust $ findIndices grid
  where
    findIndices :: [String] -> Maybe (Int, Int, Int)
    findIndices xs = listToMaybe $ do
      (y, row) <- zip [0 ..] xs
      (x, c) <- zip [0 ..] row
      case c of
        '^' -> return (x, y, 0)
        '>' -> return (x, y, 1)
        'v' -> return (x, y, 2)
        '<' -> return (x, y, 3)
        _ -> []

replace :: Point -> Char -> Array Point Char -> Array Point Char
replace (y, x) c arr = arr // [((y, x), c)]

countLoops :: Array Point Char -> Int -> Int -> Int -> Int -> Int -> Int
countLoops grid h w startX startY startDir =
  length $ filter (loops grid h w startX startY startDir) $
    [(y, x) | y <- [0 .. h - 1], x <- [0 .. w - 1], (x, y) /= (startX, startY), grid ! (y, x) == '.']

loops :: Array Point Char -> Int -> Int -> Int -> Int -> Int -> Point -> Bool
loops grid h w sx sy sdir (y, x) =
  let grid' = grid // [((y, x), '#')]
  in  simulate grid' h w sx sy sdir

simulate :: Array Point Char -> Int -> Int -> Int -> Int -> Int -> Bool
simulate grid h w sx sy sdir =
  let dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)]
      go x y dir seen step
        | step >= 2000000 = False
        | Set.member (x, y, dir) seen = True
        | otherwise =
            let (dx, dy) = dirs !! dir
                nx = x + dx
                ny = y + dy
            in if nx < 0 || nx >= w || ny < 0 || ny >= h
                then False
                else if grid ! (ny, nx) == '#'
                    then go x y ((dir + 1) `mod` 4) (Set.insert (x, y, dir) seen) (step + 1)
                    else go nx ny dir (Set.insert (x, y, dir) seen) (step + 1)
  in go sx sy sdir Set.empty 0
