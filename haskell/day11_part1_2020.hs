
import Data.List

main = do
    contents <- readFile "input.txt"
    let seatingArea = lines contents
    let stabilized = False
    let finalSeatingArea = head $ dropWhile (not . snd) $ iterate (\(seats, stable) -> simulateSeating seats stable) (seatingArea, stabilized)
    print $ countOccupiedSeats (fst finalSeatingArea)

simulateSeating :: [String] -> Bool -> ([String], Bool)
simulateSeating seatingArea stabilized = if stabilized then (seatingArea, True) else (newSeatingArea, False)
    where rows = length seatingArea
          cols = length (head seatingArea)
          newSeatingArea = [[newSeat i j | j <- [0..cols-1]] | i <- [0..rows-1]]
          stabilized = all (==True) [seatingArea !! i !! j == newSeatingArea !! i !! j | i <- [0..rows-1], j <- [0..cols-1]]
          newSeat i j = case seatingArea !! i !! j of
                            'L' -> if countAdjacentOccupied seatingArea i j == 0 then '#' else 'L'
                            '#' -> if countAdjacentOccupied seatingArea i j >= 4 then 'L' else '#'
                            _ -> seatingArea !! i !! j

countAdjacentOccupied :: [String] -> Int -> Int -> Int
countAdjacentOccupied seatingArea row col = length $ filter (== '#') [seatingArea !! i !! j | i <- [row-1..row+1], j <- [col-1..col+1], i >= 0, i < length seatingArea, j >= 0, j < length (head seatingArea), not (i == row && j == col)]

countOccupiedSeats :: [String] -> Int
countOccupiedSeats seatingArea = length $ filter (== '#') (concat seatingArea)
