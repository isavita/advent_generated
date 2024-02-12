
import System.IO

main = do
    contents <- readFile "input.txt"
    let input = head (lines contents)
        sum = foldl (\acc (x, y) -> if x == y then acc + (read [x] :: Int) else acc) 0 (zip input (tail $ cycle input))
    print sum
