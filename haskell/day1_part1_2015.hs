
import System.IO

main = do
    contents <- readFile "input.txt"
    let input = filter (`elem` "()") contents
        floor = sum [if c == '(' then 1 else -1 | c <- input]
    print floor
