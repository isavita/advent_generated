
import Data.Array
import Data.List (maximumBy)
import Data.Ord (comparing)
import System.IO
import Text.Printf (printf)

calculatePowerLevel :: Int -> Int -> Int -> Int
calculatePowerLevel x y serialNumber =
    let rackId = x + 10
        powerLevel1 = rackId * y
        powerLevel2 = powerLevel1 + serialNumber
        powerLevel3 = powerLevel2 * rackId
        powerLevel4 = (powerLevel3 `div` 100) `mod` 10
    in powerLevel4 - 5

createCumulativeGrid :: Int -> Array (Int, Int) Int
createCumulativeGrid serialNumber =
    let bounds = ((0, 0), (300, 300))
        powerGrid = array ((1, 1), (300, 300))
                          [((x, y), calculatePowerLevel x y serialNumber)
                          | x <- [1..300], y <- [1..300]]

        getCumulValue :: Int -> Int -> Int
        getCumulValue 0 _ = 0
        getCumulValue _ 0 = 0
        getCumulValue x y = powerGrid ! (x, y)
                          + cumulativeGrid ! (x - 1, y)
                          + cumulativeGrid ! (x, y - 1)
                          - cumulativeGrid ! (x - 1, y - 1)

        cumulativeGrid = array bounds
                           [((x, y), getCumulValue x y)
                           | x <- [0..300], y <- [0..300]]
    in cumulativeGrid

calculateTotalPower :: Array (Int, Int) Int -> Int -> Int -> Int -> Int
calculateTotalPower cumulativeGrid x y size =
    let x1 = x - 1
        y1 = y - 1
        x2 = x + size - 1
        y2 = y + size - 1
    in cumulativeGrid ! (x2, y2)
     - cumulativeGrid ! (x1, y2)
     - cumulativeGrid ! (x2, y1)
     + cumulativeGrid ! (x1, y1)

findLargestSquare :: Array (Int, Int) Int -> (Int, Int, Int, Int)
findLargestSquare cumulativeGrid =
    maximumBy (comparing (\(p, _, _, _) -> p)) $
    [ (calculateTotalPower cumulativeGrid x y size, x, y, size)
    | size <- [1..300]
    , x <- [1 .. 301 - size]
    , y <- [1 .. 301 - size]
    ]

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let serialNumber = read contents :: Int
    let cumulativeGrid = createCumulativeGrid serialNumber
    let (_, maxX, maxY, maxSize) = findLargestSquare cumulativeGrid
    printf "%d,%d,%d\n" maxX maxY maxSize

