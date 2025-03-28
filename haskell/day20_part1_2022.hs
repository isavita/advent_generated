
import qualified Data.Sequence as Seq
import Data.Sequence (Seq ((:<|), (:|>)), (<|), (|>))
import Data.Foldable (toList, find)
import Data.Maybe (fromJust)
import System.IO (readFile)

-- Represents an item in the list: (original index, value)
type Item = (Int, Int)

-- Moves one item in the sequence based on its value.
-- n: Original size of the list (needed for correct modulo)
-- itemToMove: The (originalIndex, value) pair to move
-- currentSeq: The current state of the sequence
-- Returns: The sequence after moving the item
mixOne :: Int -> Item -> Seq Item -> Seq Item
mixOne n itemToMove@(_, val) currentSeq =
    -- Find the current index of the item to move.
    -- Seq.elemIndexL is O(n) in the worst case, but O(log n) on average for sequences.
    -- Since we do this n times, the overall complexity with Seq operations is O(n * log n).
    let Just currentIdx = Seq.elemIndexL itemToMove currentSeq
        -- Remove the item from the sequence. O(log n)
        seqWithoutItem = Seq.deleteAt currentIdx currentSeq
        -- The length for circular wrapping is n-1 because we move *relative* to other items.
        len = n - 1
    in if len <= 0 || val == 0
         -- If list has only 1 item or value is 0, no move happens.
         then currentSeq
         else
             -- Calculate the new insertion position.
             -- The effective move distance is `val `mod` len`.
             -- We use `rem` which is Haskell's remainder operator.
             -- `(a `rem` m + m) `rem` m` ensures the result is always non-negative
             -- and within the range [0, m-1], correctly handling negative `val`.
             let insertPos = (currentIdx + val `rem` len + len) `rem` len
             -- Insert the item at the calculated position. O(log n)
             in Seq.insertAt insertPos itemToMove seqWithoutItem

-- Performs the full mixing process on the initial list.
-- n: Original size of the list
-- originalOrder: List of (originalIndex, value) pairs in their initial order
-- Returns: The final sequence after one round of mixing
mix :: Int -> [Item] -> Seq Item
mix n originalOrder =
    -- Use foldl to iterate through the original order, applying mixOne to the sequence state.
    -- The initial state is the sequence created from the original order.
    foldl (\currentSeq item -> mixOne n item currentSeq) (Seq.fromList originalOrder) originalOrder

-- Finds the grove coordinates from the final mixed sequence.
getCoordinates :: Seq Item -> Int
getCoordinates finalSeq =
    -- Find the index of the item with value 0. O(n)
    let Just zeroIndex = Seq.findIndexL ((== 0) . snd) finalSeq
        n = Seq.length finalSeq
        -- Helper to get the value at a specific index, wrapping around. O(log n)
        getValueAt idx = snd $ Seq.index finalSeq idx
        -- Calculate the indices 1000, 2000, 3000 steps after the zero index, wrapping around.
        idx1000 = (zeroIndex + 1000) `mod` n
        idx2000 = (zeroIndex + 2000) `mod` n
        idx3000 = (zeroIndex + 3000) `mod` n
    -- Sum the values at these three indices.
    in getValueAt idx1000 + getValueAt idx2000 + getValueAt idx3000

-- Main entry point
main :: IO ()
main = do
    -- Read input from the file "input.txt"
    inputText <- readFile "input.txt"
    -- Parse lines into a list of integers
    -- Using read :: String -> Int, requires input lines to be valid integers.
    let numbers = map read (lines inputText) :: [Int]
        n = length numbers

    -- Check for empty input, though problem implies non-empty.
    if n == 0
    then putStrLn "Input file is empty."
    else do
        -- Pair numbers with their original indices (0 to n-1) to distinguish identical values.
        let originalOrder = zip [0..] numbers

        -- Perform the mixing process exactly once.
        let finalSeq = mix n originalOrder

        -- Calculate the sum of the grove coordinates.
        let result = getCoordinates finalSeq

        -- Print the result to standard output.
        print result

