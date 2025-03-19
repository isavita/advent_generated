
import Data.List (foldl')
import qualified Data.Map as M

type Point = (Int, Int)
type Grid = M.Map Point Char

process :: [String] -> [Int]
process = foldl' go [1]
  where
    go xs "noop" = xs ++ [last xs]
    go xs line =
      let n = read (last (words line)) :: Int
       in xs ++ [last xs, last xs + n]

draw :: Grid -> Int -> IO ()
draw grid h =
    mapM_ putStrLn [ [ M.findWithDefault '.' (x,y) grid | x <- [0..39] ] | y <- [0..h-1] ]

solve :: [String] -> IO ()
solve input = do
  let x = process input
      grid = M.fromList [((crtx, crty), if abs (crtx - val) <= 1 then '#' else '.') | (i, val) <- zip [0..] x, let crtx = i `mod` 40, let crty = i `div` 40]
  draw grid 6

main :: IO ()
main = do
  input <- readFile "input.txt"
  solve (lines input)
