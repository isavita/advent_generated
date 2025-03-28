
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict (State, gets, modify, runState)
import System.IO (readFile)
import Text.Read (readMaybe) -- Using readMaybe for safer parsing if needed, though not strictly required here

-- Type alias for the memoization map: ((steps_remaining, stone_value), count)
type Memo = Map.Map (Int, Integer) Integer
-- Type alias for the state monad holding the memo map
type CountS = State Memo Integer

-- Function implementing the rules for a single stone transformation
-- Returns a list of the resulting stone values
applyRule :: Integer -> [Integer]
applyRule 0 = [1] -- Rule 1: 0 becomes 1
applyRule n =
    let s = show n
        len = length s
    in if even len
       then -- Rule 2: Even digits, split
            let (leftS, rightS) = splitAt (len `div` 2) s
                -- Use readMaybe for robustness, defaulting to 0 if parse fails (though unlikely here)
                leftVal  = maybe 0 id (readMaybe leftS)
                rightVal = maybe 0 id (readMaybe rightS)
            in [leftVal, rightVal]
       else -- Rule 3: Odd digits, multiply by 2024
            [n * 2024]

-- Function to calculate the number of descendant stones after 'k' steps using memoization
-- Uses the State monad to manage the memoization map implicitly
descendantCount :: Int -> Integer -> CountS
descendantCount 0 _ = return 1 -- Base case: 0 steps remaining, the stone itself is 1 descendant
descendantCount k n = do
    -- Check if the result for (k, n) is already memoized
    memo <- gets (Map.lookup (k, n))
    case memo of
        Just count -> return count -- Return memoized result
        Nothing -> do
            -- If not memoized, apply the rule to get the next state stones
            let nextStones = applyRule n
            -- Recursively calculate descendant counts for each resulting stone after k-1 steps
            -- mapM sequences the stateful computations, threading the memo map
            counts <- mapM (descendantCount (k - 1)) nextStones
            -- The total count for (k, n) is the sum of descendants from the next state
            let totalCount = sum counts
            -- Store the newly computed result in the memo map
            modify (Map.insert (k, n) totalCount)
            -- Return the computed result
            return totalCount

-- Function to calculate the total number of stones after 'k' steps for a list of initial stones
calculateTotalCount :: Int -> [Integer] -> Integer
calculateTotalCount k initialStones =
    -- Initialize the computation with an empty memo map
    -- mapM runs descendantCount for each initial stone, accumulating state changes (memo updates)
    -- runState executes the stateful computation and returns the final results and the final state (memo map)
    let computation = mapM (descendantCount k) initialStones
        (counts, _finalMemo) = runState computation Map.empty
    -- Sum the counts obtained for each initial stone
    in sum counts

-- Main function: Reads input, performs calculations, and prints results
main :: IO ()
main = do
    -- Read the contents of input.txt
    contents <- readFile "input.txt"
    -- Parse the space-separated numbers into a list of Integers
    -- Using Integer to handle potentially very large numbers
    let initialStones = map read (words contents) :: [Integer]

    -- Calculate and print the result for Part 1 (25 blinks)
    let numBlinks1 = 25
    let totalCount1 = calculateTotalCount numBlinks1 initialStones
    putStrLn $ "Part 1: " ++ show totalCount1

    -- Calculate and print the result for Part 2 (75 blinks)
    -- The memoization ensures that computations are reused efficiently
    let numBlinks2 = 75
    let totalCount2 = calculateTotalCount numBlinks2 initialStones
    putStrLn $ "Part 2: " ++ show totalCount2
