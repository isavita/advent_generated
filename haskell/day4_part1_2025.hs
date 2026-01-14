
import Data.List (transpose)
import System.IO (readFile)

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  let grid = filter (not . null) ls
      rows = length grid
      cols = if null grid then 0 else length (head grid)
      dirs = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
      acc = sum [ 1 | y <- [0..rows-1],
                      x <- [0..cols-1],
                      grid!!y!!x == '@',
                      length [ () | (dy,dx) <- dirs,
                                    let ny = y+dy
                                        nx = x+dx,
                                    ny >= 0, ny < rows,
                                    nx >= 0, nx < cols,
                                    grid!!ny!!nx == '@' ] < 4 ]
  print acc
