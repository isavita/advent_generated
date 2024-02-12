
import System.IO
import Data.List

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let grid = lines contents
        Just x = elemIndex '|' (head grid)
        (steps, _, _) = walkPath grid x 0 0 1 0
    print steps

walkPath :: [String] -> Int -> Int -> Int -> Int -> Int -> (Int, Int, Int)
walkPath grid x y dx dy steps
    | x < 0 || x >= length (head grid) || y < 0 || y >= length grid = (steps, x, y)
    | cell == ' ' = (steps, x, y)
    | cell == '+' = if dx == 0
                    then if x > 0 && (grid !! y !! (x-1) == '-' || isLetter (grid !! y !! (x-1)))
                         then walkPath grid (x-1) y (-1) 0 (steps+1)
                         else walkPath grid (x+1) y 1 0 (steps+1)
                    else if y > 0 && (grid !! (y-1) !! x == '|' || isLetter (grid !! (y-1) !! x))
                         then walkPath grid x (y-1) 0 (-1) (steps+1)
                         else walkPath grid x (y+1) 0 1 (steps+1)
    | otherwise = walkPath grid (x+dx) (y+dy) dx dy (steps+1)
    where cell = grid !! y !! x

isLetter :: Char -> Bool
isLetter c = c >= 'A' && c <= 'Z'
