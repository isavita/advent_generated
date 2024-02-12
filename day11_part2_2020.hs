
import System.IO

data Point = Point { x :: Int, y :: Int }

directions :: [Point]
directions = [Point (-1) (-1), Point 0 (-1), Point 1 (-1), Point (-1) 0, Point 1 0, Point (-1) 1, Point 0 1, Point 1 1]

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let seatingArea = lines contents
    let stabilized = False
    let finalSeatingArea = simulateSeatingPartTwo seatingArea stabilized
    print $ countOccupiedSeats finalSeatingArea

simulateSeatingPartTwo :: [String] -> Bool -> [String]
simulateSeatingPartTwo seatingArea stabilized
    | stabilized = seatingArea
    | otherwise = simulateSeatingPartTwo newSeatingArea newStabilized
    where
        rows = length seatingArea
        cols = length (head seatingArea)
        newSeatingArea = map (\(i, row) -> map (\(j, seat) -> updateSeat seatingArea i j seat) (zip [0..] row)) (zip [0..] seatingArea)
        newStabilized = all (== True) [seatingArea !! i !! j == newSeatingArea !! i !! j | i <- [0..rows-1], j <- [0..cols-1]]

updateSeat :: [String] -> Int -> Int -> Char -> Char
updateSeat seatingArea i j seat
    | seat == 'L' && countVisibleOccupied seatingArea i j == 0 = '#'
    | seat == '#' && countVisibleOccupied seatingArea i j >= 5 = 'L'
    | otherwise = seat

countVisibleOccupied :: [String] -> Int -> Int -> Int
countVisibleOccupied seatingArea row col = length $ filter (== '#') [getVisibleSeat seatingArea row col dir | dir <- directions]

getVisibleSeat :: [String] -> Int -> Int -> Point -> Char
getVisibleSeat seatingArea row col dir
    | r < 0 || r >= length seatingArea || c < 0 || c >= length (head seatingArea) = '.'
    | seat == 'L' || seat == '#' = seat
    | otherwise = getVisibleSeat seatingArea r c dir
    where
        r = row + y dir
        c = col + x dir
        seat = seatingArea !! r !! c

countOccupiedSeats :: [String] -> Int
countOccupiedSeats seatingArea = length $ filter (== '#') (concat seatingArea)
