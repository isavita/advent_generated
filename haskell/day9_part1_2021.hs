
import System.IO

main = do
    contents <- readFile "input.txt"
    let heightmap = map (map (\c -> read [c])) $ lines contents
    let totalRiskLevel = sum [1 + height | (y, row) <- zip [0..] heightmap, (x, height) <- zip [0..] row, isLowPoint heightmap x y]
    print totalRiskLevel

isLowPoint :: [[Int]] -> Int -> Int -> Bool
isLowPoint heightmap x y = 
    let height = (heightmap !! y) !! x
    in (x == 0 || (heightmap !! y) !! (x-1) > height) &&
       (x == length (heightmap !! y) - 1 || (heightmap !! y) !! (x+1) > height) &&
       (y == 0 || (heightmap !! (y-1)) !! x > height) &&
       (y == length heightmap - 1 || (heightmap !! (y+1)) !! x > height)
