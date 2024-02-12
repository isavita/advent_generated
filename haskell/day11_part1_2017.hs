
import System.IO

abs' x = if x < 0 then -x else x

max' a b = if a > b then a else b

distance x y z = (abs' x + abs' y + abs' z) `div` 2

main = do
    contents <- readFile "input.txt"
    let directions = words (map (\c -> if c == ',' then ' ' else c) contents)
        move "n"  (x, y, z) = (x, y+1, z-1)
        move "ne" (x, y, z) = (x+1, y, z-1)
        move "se" (x, y, z) = (x+1, y-1, z)
        move "s"  (x, y, z) = (x, y-1, z+1)
        move "sw" (x, y, z) = (x-1, y, z+1)
        move "nw" (x, y, z) = (x-1, y+1, z)
        updatePos (x, y, z) dir = move dir (x, y, z)
        (x, y, z) = foldl (\pos dir -> updatePos pos dir) (0, 0, 0) directions
    print (distance x y z)
