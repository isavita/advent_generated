
import Data.List

main = do
    input <- readFile "input.txt"
    let santaMoves = map fst $ filter (\(_,i) -> even i) $ zip input [0..]
        roboMoves = map fst $ filter (\(_,i) -> odd i) $ zip input [0..]
        santaHouses = nub $ scanl move (0,0) santaMoves
        roboHouses = nub $ scanl move (0,0) roboMoves
        allHouses = nub $ santaHouses ++ roboHouses
    print $ length allHouses

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) '^' = (x, y+1)
move (x, y) 'v' = (x, y-1)
move (x, y) '>' = (x+1, y)
move (x, y) '<' = (x-1, y)
