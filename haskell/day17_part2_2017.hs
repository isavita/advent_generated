
{-# LANGUAGE BangPatterns #-}

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|), (|>), (><))
import Data.Foldable (toList)
import System.IO (readFile)

-- | Represents the state of the spinlock simulation for Part 1
--   (buffer, current_position_index)
type SpinState1 = (Seq Int, Int)

-- | Represents the state of the spinlock simulation for Part 2
--   (current_position_index, current_buffer_size, value_after_zero)
type SpinState2 = (Int, Int, Int)

-- | Performs one step of the spinlock insertion for Part 1
step1 :: Int -> SpinState1 -> Int -> SpinState1
step1 !steps (!buffer, !currentPos) !valueToInsert =
    let bufferSize = Seq.length buffer
        -- Calculate the position *before* the insertion point
        insertAfterPos = (currentPos + steps) `mod` bufferSize
        -- The new value is inserted *after* insertAfterPos
        newCurrentPos = insertAfterPos + 1
        -- Split the sequence and insert the new value
        (before, after) = Seq.splitAt newCurrentPos buffer
        newBuffer = before >< (valueToInsert Seq.<| after)
    in (newBuffer, newCurrentPos)

-- | Solves Part 1 of the challenge
solvePart1 :: Int -> Int -> Int
solvePart1 steps numInsertions =
    let initialBuffer = Seq.singleton 0
        initialState = (initialBuffer, 0) :: SpinState1
        -- Run the simulation for numInsertions steps
        (finalBuffer, _) = foldl' (step1 steps) initialState [1..numInsertions]
        -- Find the index of the last inserted value (numInsertions)
        Just indexOfLast = Seq.findIndexL (== numInsertions) finalBuffer
        -- Get the index of the element immediately after it (circularly)
        indexAfterLast = (indexOfLast + 1) `mod` Seq.length finalBuffer
        -- Retrieve the value at that index
    in Seq.index finalBuffer indexAfterLast

-- | Performs one step of the optimized spinlock simulation for Part 2
--   Only tracks necessary information: current position, size, and value after 0
step2 :: Int -> SpinState2 -> Int -> SpinState2
step2 !steps (!currentPos, !bufferSize, !valueAfterZero) !valueToInsert =
    let -- Calculate the position *before* the insertion point
        insertAfterPos = (currentPos + steps) `mod` bufferSize
        -- The new value's position will be insertAfterPos + 1
        newCurrentPos = insertAfterPos + 1
        newBufferSize = bufferSize + 1
        -- Update valueAfterZero ONLY if the insertion happens right after index 0
        newValueAfterZero = if insertAfterPos == 0 then valueToInsert else valueAfterZero
    in (newCurrentPos, newBufferSize, newValueAfterZero)

-- | Solves Part 2 of the challenge using the optimized O(N) time, O(1) space approach
solvePart2 :: Int -> Int -> Int
solvePart2 steps numInsertions =
    let initialState = (0, 1, 0) :: SpinState2 -- (pos=0, size=1, valAfter0=N/A initially, use 0 placeholder)
        -- Run the optimized simulation
        (_, _, finalValueAfterZero) = foldl' (step2 steps) initialState [1..numInsertions]
    in finalValueAfterZero

-- | Main entry point
main :: IO ()
main = do
    -- Read the step value from input.txt
    inputContent <- readFile "input.txt"
    let steps = read (filter (/= '\n') inputContent) :: Int

    -- Calculate and print Part 1 result
    let result1 = solvePart1 steps 2017
    putStrLn $ "Part 1: " ++ show result1

    -- Calculate and print Part 2 result
    let result2 = solvePart2 steps 50000000
    putStrLn $ "Part 2: " ++ show result2

-- | Strict left fold for efficiency, especially important for Part 2
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z []     = z
foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs
