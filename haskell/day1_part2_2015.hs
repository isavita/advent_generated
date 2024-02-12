
import System.IO

main = do
    input <- readFile "input.txt"
    let position = length $ takeWhile (/= -1) $ scanl (\acc c -> if c == '(' then acc + 1 else acc - 1) 0 input
    print position
