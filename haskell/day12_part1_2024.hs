
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, ViewL(..), (|>))
import qualified Data.Array.ST as STArray
import Data.Array.ST (STUArray)
import qualified Data.Array as Array
import Data.Array (Array, (!), bounds, listArray)
import Control.Monad (foldM, filterM, when)
import Control.Monad.ST (ST, runST)
import System.IO (readFile)

-- Performs Breadth-First Search to find the area and perimeter of a connected region
bfs :: Array (Int, Int) Char -> STUArray s (Int, Int) Bool -> Int -> Int -> (Int, Int) -> ST s (Int, Int)
bfs grid visited rows cols start@(startR, startC) = do
    let targetChar = grid ! start
    -- Mark start as visited and initialize queue
    STArray.writeArray visited start True
    let initialQueue = Seq.singleton start

    -- Recursive loop processing the queue
    let loop queue currentArea currentPerimeter =
            case Seq.viewl queue of
                EmptyL -> return (currentArea, currentPerimeter) -- Base case: Queue empty
                (r, c) :< restQueue -> do
                    -- Calculate perimeter contribution for this cell
                    let cellPerimeter = calculatePerimeterForCell grid targetChar rows cols (r, c)

                    -- Find valid neighbors (in bounds, same char, not visited)
                    validNeighbors <- filterM (isValidNeighbor grid visited targetChar rows cols) (potentialNeighbors (r, c))

                    -- Mark valid neighbors as visited
                    mapM_ (\coord -> STArray.writeArray visited coord True) validNeighbors

                    -- Add valid neighbors to the queue
                    let newQueue = foldl (|>) restQueue validNeighbors

                    -- Recurse with updated area, perimeter, and queue
                    loop newQueue (currentArea + 1) (currentPerimeter + cellPerimeter)

    loop initialQueue 0 0 -- Start the loop

-- Generates potential neighbor coordinates (may be out of bounds)
potentialNeighbors :: (Int, Int) -> [(Int, Int)]
potentialNeighbors (r, c) = [(r + dr, c + dc) | (dr, dc) <- [(0, 1), (0, -1), (1, 0), (-1, 0)]]

-- Checks if a potential neighbor is valid for adding to the BFS queue
isValidNeighbor :: Array (Int, Int) Char -> STUArray s (Int, Int) Bool -> Char -> Int -> Int -> (Int, Int) -> ST s Bool
isValidNeighbor grid visited targetChar rows cols (nr, nc) = do
    if nr < 0 || nr >= rows || nc < 0 || nc >= cols then return False -- Out of bounds
    else do
        let neighborChar = grid ! (nr, nc)
        if neighborChar /= targetChar then return False -- Different character
        else do
            isVisited <- STArray.readArray visited (nr, nc)
            return $ not isVisited -- Return true only if same char and not visited

-- Calculates the perimeter contribution of a single cell
calculatePerimeterForCell :: Array (Int, Int) Char -> Char -> Int -> Int -> (Int, Int) -> Int
calculatePerimeterForCell grid targetChar rows cols (r, c) =
    sum $ map checkNeighbor (potentialNeighbors (r, c))
    where
        checkNeighbor (nr, nc)
            | nr < 0 || nr >= rows || nc < 0 || nc >= cols = 1 -- Out of bounds
            | grid ! (nr, nc) /= targetChar = 1             -- Different char
            | otherwise = 0                                 -- Same char

-- Main solver function
solve :: [String] -> Int
solve gridLines =
    let rows = length gridLines
        cols = if rows == 0 then 0 else length (head gridLines)
        gridBounds = ((0, 0), (rows - 1, cols - 1))
    in if rows == 0 || cols == 0 then 0 else
        let grid = listArray gridBounds [gridLines !! r !! c | r <- [0..rows-1], c <- [0..cols-1]]
        in runST $ do
            -- Create mutable visited array initialized to False
            visited <- STArray.newArray gridBounds False :: ST s (STUArray s (Int, Int) Bool)

            -- Iterate through all cells, accumulating total price
            let processCell currentTotal (r, c) = do
                    isVisited <- STArray.readArray visited (r, c)
                    if isVisited then
                        return currentTotal -- Skip if already part of a visited region
                    else do
                        -- Perform BFS to find region area and perimeter
                        (area, perimeter) <- bfs grid visited rows cols (r, c)
                        return $ currentTotal + area * perimeter -- Add region's contribution

            -- Use foldM to process all coordinates
            foldM processCell 0 (Array.range gridBounds)

-- Main entry point
main :: IO ()
main = do
    contents <- readFile "input.txt"
    let grid = lines contents
    print $ solve grid
