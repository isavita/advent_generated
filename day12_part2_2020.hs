
import System.IO

data Ship = Ship { x :: Int, y :: Int, waypointX :: Int, waypointY :: Int }

main = do
    contents <- readFile "input.txt"
    let instructions = lines contents
        ship = Ship 0 0 10 1
        finalShip = foldl processInstruction ship instructions
        manhattanDistance = myAbs (x finalShip) + myAbs (y finalShip)
    print manhattanDistance

processInstruction :: Ship -> String -> Ship
processInstruction ship instruction = case (head instruction, read (tail instruction) :: Int) of
    ('N', value) -> ship { waypointY = waypointY ship + value }
    ('S', value) -> ship { waypointY = waypointY ship - value }
    ('E', value) -> ship { waypointX = waypointX ship + value }
    ('W', value) -> ship { waypointX = waypointX ship - value }
    ('L', value) -> rotateWaypoint ship (-value)
    ('R', value) -> rotateWaypoint ship value
    ('F', value) -> ship { x = x ship + waypointX ship * value, y = y ship + waypointY ship * value }

rotateWaypoint :: Ship -> Int -> Ship
rotateWaypoint ship degrees = case mod (degrees + 360) 360 of
    90 -> ship { waypointX = waypointY ship, waypointY = -waypointX ship }
    180 -> ship { waypointX = -waypointX ship, waypointY = -waypointY ship }
    270 -> ship { waypointX = -waypointY ship, waypointY = waypointX ship }
    _ -> ship

myAbs :: Int -> Int
myAbs x = if x < 0 then -x else x
