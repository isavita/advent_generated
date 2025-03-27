
import System.IO
import qualified Data.Set as Set
import Data.Set (Set)

type Point = (Int, Int)
type Garden = Set Point

posMod :: Int -> Int -> Int
posMod x m = (x `mod` m + m) `mod` m

parseData :: [String] -> (Garden, Point, Int)
parseData gridLines =
    let height = length gridLines
        width = if height > 0 then length (head gridLines) else 0
        maxSize = height -- Assuming square grid based on problem context
        (gardenSet, maybeStart) = foldl processLine (Set.empty, Nothing) (zip [0..] gridLines)

        processLine (gardenAcc, startAcc) (y, line) =
          foldl (processChar y) (gardenAcc, startAcc) (zip [0..] line)

        processChar y (gardenAcc, startAcc) (x, c) =
          let currentPoint = (x, y)
          in case c of
               '#' -> (gardenAcc, startAcc)
               'S' -> (Set.insert currentPoint gardenAcc, Just currentPoint)
               '.' -> (Set.insert currentPoint gardenAcc, startAcc)
               _   -> error "Invalid character in grid"

    in case maybeStart of
         Just start -> (gardenSet, start, maxSize)
         Nothing    -> error "No start 'S' found!"

calculateNumEnds :: Garden -> Point -> Int -> Int -> Int
calculateNumEnds garden start numIterations maxSize =
    let targetN = numIterations `div` maxSize
        collectRem = (maxSize - 1) `div` 2

        isValidNeighbor :: Point -> Bool
        isValidNeighbor (nx, ny) =
            Set.member (nx `posMod` maxSize, ny `posMod` maxSize) garden

        neighbors :: Point -> Set Point
        neighbors (x, y) =
            Set.fromList $ filter isValidNeighbor
                [ (x + dx, y + dy) | (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)] ]

        step :: Set Point -> Set Point
        step currentQueue = Set.unions $ Set.map neighbors currentQueue

        go :: Int -> Set Point -> [Int] -> [Int]
        go i currentQueue collectedCounts
          | length collectedCounts == 3 = collectedCounts
          | otherwise =
              let !newQueue = step currentQueue -- Use BangPattern for strictness
                  newCollected = if i >= collectRem && (i - collectRem) `mod` maxSize == 0
                                 then collectedCounts ++ [Set.size currentQueue]
                                 else collectedCounts
              in go (i + 1) newQueue newCollected

        done = go 0 (Set.singleton start) []

        (a:b:c:_) = map fromIntegral done -- Use Integer for quadratic calculation

        -- Use Integer to avoid overflow issues with large numbers
        n :: Integer
        n = fromIntegral targetN

        quadraticFn :: Integer -> Integer -> Integer -> Integer -> Integer
        quadraticFn n' a' b' c' =
             a' + n' * (b' - a' + (n' - 1) * (c' - 2 * b' + a') `div` 2)

    in fromIntegral (quadraticFn n a b c)


main :: IO ()
main = do
    contents <- readFile "input.txt"
    let inputLines = lines contents
        (garden, start, maxSize) = parseData inputLines
        targetSteps = 26501365
        result = calculateNumEnds garden start targetSteps maxSize
    print result
