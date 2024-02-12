
import System.IO
import Data.List
import Data.Char
import Data.Function

main = do
    contents <- readFile "input.txt"
    let target = read (head (lines contents)) :: Int
        sideLength = ceiling (sqrt (fromIntegral target))
        sideLength' = if even sideLength then sideLength + 1 else sideLength
        maxValue = sideLength' * sideLength'
        stepsFromEdge = (sideLength' - 1) `div` 2
        middlePoints = map (\i -> maxValue - stepsFromEdge - (sideLength' - 1) * i) [0..3]
        distanceToMiddle = minimumBy (compare `on` (abs . (target -))) middlePoints
        manhattanDistance = stepsFromEdge + abs (target - distanceToMiddle)
    print manhattanDistance
