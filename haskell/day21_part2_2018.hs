
import Data.Bits
import qualified Data.Set as Set

-- Simulates the inner loop until r3 < 256, returning the final r5
runInnerLoop :: Int -> Int -> Int
runInnerLoop r3 r5
  | r3 < 256 = nextR5
  | otherwise = runInnerLoop nextR3 nextR5
  where
    r1 = r3 .&. 255
    nextR5 = ((r5 + r1) .&. 16777215) * 65899 .&. 16777215
    -- Calculate nextR3 only if needed (avoids calculation in base case)
    nextR3 = r3 `div` 256

-- Finds the last unique value before a cycle is detected
findLastUnique :: Set.Set Int -> Int -> Int
findLastUnique seen prevR5 =
    let r3 = prevR5 .|. 65536
        currentR5 = runInnerLoop r3 7586220 -- Calculate the next value in the sequence
    in if Set.member currentR5 seen
       then prevR5 -- Cycle detected, the previous value was the last unique one
       else findLastUnique (Set.insert currentR5 seen) currentR5 -- Continue search

solve :: Int
solve =
    let firstR5 = runInnerLoop (0 .|. 65536) 7586220 -- Calculate the first value (Part 1 answer, though not printed)
        initialSeen = Set.singleton firstR5
    in findLastUnique initialSeen firstR5 -- Start search for Part 2 answer

main :: IO ()
main = do
    -- The provided Python code doesn't use input.txt, so we directly compute.
    let result = solve
    print result
