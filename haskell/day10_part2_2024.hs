
import Data.Array
import Data.Char (digitToInt)

solve :: [[Int]] -> Int
solve grid = total
  where
    nr = length grid
    nc = length (head grid)
    bounds = ((0, 0), (nr - 1, nc - 1))
    grid' = listArray bounds (concat grid)
    dp = array bounds [((r, c), -1) | r <- [0 .. nr - 1], c <- [0 .. nc - 1]]
    dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

    dfs :: (Int, Int) -> Array (Int, Int) Int -> Int
    dfs (r, c) dp'
      | val /= -1 = val
      | h == 9 = 1
      | otherwise = sumPaths
      where
        val = dp' ! (r, c)
        h = grid' ! (r, c)
        neighbors = filter (\(nr2, nc2) -> inRange bounds (nr2, nc2) && grid' ! (nr2, nc2) == h + 1)
                    [(r + dr, c + dc) | (dr, dc) <- dirs]
        sumPaths = sum [dfs (nr2, nc2) dp' | (nr2, nc2) <- neighbors]
        dp'' = dp' // [((r, c), sumPaths)]

    results = map (\r -> map (\c -> if grid' ! (r,c) == 0 then dfs (r,c) dp else 0) [0..nc-1]) [0..nr-1]

    total = sum (map sum results)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let grid = map (map digitToInt) (lines contents)
  print (solve grid)
