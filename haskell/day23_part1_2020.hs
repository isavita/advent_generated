
import Data.Char (digitToInt)
import Data.Array.ST (STUArray, newArray, readArray, writeArray, runSTUArray, freeze)
import Data.Array.Unboxed (UArray, (!), bounds)
import Control.Monad (foldM_, when)
import Control.Monad.ST (ST)
import System.IO (readFile)

-- | Parses the input string into a list of integers.
parseInput :: String -> [Int]
parseInput = map digitToInt . head . lines

-- | Creates the initial circle representation using a mutable array.
-- The array maps each cup label to the label of the cup immediately clockwise.
createCircle :: [Int] -> (Int, Int) -> ST s (STUArray s Int Int)
createCircle cups (minLabel, maxLabel) = do
    -- Create an array to store the next cup for each label.
    -- Bounds are (minLabel, maxLabel).
    arr <- newArray (minLabel, maxLabel) 0 :: ST s (STUArray s Int Int)

    -- Link the cups based on the input order.
    let linkPairs (x:y:rest) = writeArray arr x y >> linkPairs (y:rest)
        linkPairs [_]        = return ()
        linkPairs []         = return () -- Should not happen with valid input

    linkPairs cups

    -- Link the last cup back to the first cup to close the circle.
    when (not (null cups)) $
      writeArray arr (last cups) (head cups)

    return arr

-- | Performs a single move of the crab game.
performMove :: STUArray s Int Int -> Int -> (Int, Int) -> ST s Int
performMove arr currentCup (minLabel, maxLabel) = do
    -- 1. Pick up the three cups immediately clockwise of the current cup.
    p1 <- readArray arr currentCup
    p2 <- readArray arr p1
    p3 <- readArray arr p2
    let pickedUp = [p1, p2, p3]

    -- The cup that will follow the current cup after picking up p1, p2, p3
    nextAfterP3 <- readArray arr p3

    -- Remove the three cups by linking currentCup to the cup after p3.
    writeArray arr currentCup nextAfterP3

    -- 2. Select the destination cup.
    let findDestination dest
          | dest < minLabel = findDestination maxLabel -- Wrap around if below min label
          | dest `elem` pickedUp = findDestination (dest - 1) -- Keep subtracting if picked up
          | otherwise = dest -- Found the destination
    let destinationCup = findDestination (currentCup - 1)

    -- 3. Place the picked-up cups immediately clockwise of the destination cup.
    -- Get the cup currently after the destination cup.
    nextAfterDest <- readArray arr destinationCup

    -- Insert p1 after destinationCup.
    writeArray arr destinationCup p1
    -- Insert the rest after p1 (p3 points to the original nextAfterDest).
    writeArray arr p3 nextAfterDest

    -- 4. Select the new current cup (the one clockwise of the original current cup).
    -- This is the cup that was originally after p3 before insertion.
    return nextAfterP3 -- This was calculated earlier

-- | Generates the result string starting from the cup after 1.
getResult :: UArray Int Int -> String
getResult finalArr = go (finalArr ! 1) ""
  where
    go currentCup acc
      | currentCup == 1 = acc -- Stop when we loop back to 1
      | otherwise = go (finalArr ! currentCup) (acc ++ show currentCup)

-- | Main function
main :: IO ()
main = do
    -- Read input from file
    inputStr <- readFile "input.txt"
    let initialCups = parseInput inputStr

    -- Determine label bounds
    let minLabel = minimum initialCups
    let maxLabel = maximum initialCups
    let labelBounds = (minLabel, maxLabel)

    -- Number of moves
    let numMoves = 100

    -- Run the simulation using ST monad for efficient mutable arrays
    let finalArray = runSTUArray $ do
            arr <- createCircle initialCups labelBounds
            -- Perform moves, updating the current cup each time
            foldM_ (\current _ -> performMove arr current labelBounds) (head initialCups) [1..numMoves]
            -- Return the final state of the array (frozen)
            return arr

    -- Generate the result string
    let result = getResult finalArray

    -- Print the result to standard output
    putStrLn result
