
import Data.Array.IO
import Data.Char (digitToInt)
import Control.Monad (foldM, when)
import System.IO (readFile)

totalCups :: Int
totalCups = 1000000

totalMoves :: Int
totalMoves = 10000000

main :: IO ()
main = do
    inputStr <- head . lines <$> readFile "input.txt"
    let initialCups = map digitToInt inputStr
    
    cups <- newArray (1, totalCups) 0 :: IO (IOUArray Int Int)
    
    let n = length initialCups
        firstCup = head initialCups

    -- Link input cups
    lastInputCup <- foldM (\prev curr -> writeArray cups prev curr >> return curr) firstCup (tail initialCups)

    -- Link subsequent cups up to totalCups, starting from n+1
    lastCupBeforeLoop <- if n < totalCups then do
                            writeArray cups lastInputCup (n + 1)
                            foldM (\prev i -> writeArray cups prev i >> return i) (n + 1) [n + 2 .. totalCups]
                         else
                            return lastInputCup

    -- Link the very last cup back to the first
    writeArray cups lastCupBeforeLoop firstCup

    -- Run simulation
    runSimulation cups firstCup totalMoves

    -- Calculate and print result
    cup1 <- readArray cups 1
    cup2 <- readArray cups cup1
    print (fromIntegral cup1 * fromIntegral cup2 :: Integer)


runSimulation :: IOUArray Int Int -> Int -> Int -> IO ()
runSimulation cups currentCup movesLeft
    | movesLeft == 0 = return ()
    | otherwise = do
        -- Pick up cups
        pickup1 <- readArray cups currentCup
        pickup2 <- readArray cups pickup1
        pickup3 <- readArray cups pickup2

        -- Remove pickups by linking current to the one after pickup3
        nextAfterPickup3 <- readArray cups pickup3
        writeArray cups currentCup nextAfterPickup3

        -- Find destination cup
        let findDest dest
                | dest == 0 = findDest totalCups -- Wrap around
                | dest == pickup1 || dest == pickup2 || dest == pickup3 = findDest (dest - 1)
                | otherwise = return dest
        destinationCup <- findDest (if currentCup == 1 then totalCups else currentCup - 1)

        -- Insert pickups after destination cup
        nextAfterDest <- readArray cups destinationCup
        writeArray cups destinationCup pickup1
        writeArray cups pickup3 nextAfterDest

        -- Advance to the next current cup
        nextCurrentCup <- readArray cups currentCup 

        -- Recurse
        runSimulation cups nextCurrentCup (movesLeft - 1)

