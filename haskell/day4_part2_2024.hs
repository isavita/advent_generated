
module Main where

import Data.List (transpose)

check :: String -> Bool
check "MAS" = True
check "SAM" = True
check _     = False

solve :: [String] -> Int
solve grid = length $ filter id [ check (d1 x y) && check (d2 x y) | x <- [1..r-2], y <- [1..c-2], grid !! x !! y == 'A']
    where
        r = length grid
        c = length (head grid)
        d1 x y = [grid !! (x-1) !! (y-1), grid !! x !! y, grid !! (x+1) !! (y+1)]
        d2 x y = [grid !! (x-1) !! (y+1), grid !! x !! y, grid !! (x+1) !! (y-1)]


main :: IO ()
main = do
    content <- readFile "input.txt"
    let grid = lines content
    print $ solve grid
