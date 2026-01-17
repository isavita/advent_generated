
import qualified Data.IntSet as IntSet
import Data.Bits (xor, bit)
import Data.List (foldl', nub)
import System.IO

-- The type for a machine: (Target bitmask, Button effect bitmasks)
type Machine = (Int, [Int])

-- | Splits a string by a given delimiter character.
splitBy :: Char -> String -> [String]
splitBy delimiter str = 
    let (w, s'') = break (== delimiter) str
    in if null s'' then [w] else w : splitBy delimiter (tail s'')

-- | Parses a line from the input file into a Machine configuration.
-- Example line: [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
parseLine :: String -> Machine
parseLine s =
    let (diagPart, rest) = break (== ' ') s
        diagram = filter (`elem` ".#") diagPart
        -- The machine has lights starting from index 0. '#' represents ON.
        targetVal = foldl' (\acc (i, c) -> if c == '#' then acc `xor` (bit i) else acc) 0 (zip [0..] diagram)
        
        -- Recursively extract content between parentheses to find button bitmasks
        getButtons [] = []
        getButtons ('(':xs) = 
            let (inside, remaining) = break (== ')') xs
                -- Extract light indices from (0,1,2) format
                nums = filter (not . null) $ splitBy ',' (filter (`elem` "0123456789,") inside)
                btnVal = foldl' (\acc nStr -> acc `xor` (bit (read nStr))) 0 nums
            in btnVal : getButtons (drop 1 remaining)
        getButtons (_:xs) = getButtons xs

        -- Ignore joltage requirements inside curly braces
        btnPart = takeWhile (/= '{') rest
        buttonVals = nub $ getButtons btnPart
    in (targetVal, buttonVals)

-- | Solves the shortest path problem for light configurations using BFS.
-- Since toggling is XOR and we want the minimum presses, this is equivalent
-- to finding the shortest path in a graph where nodes are configurations.
solveMachine :: Machine -> Int
solveMachine (target, btns)
    | target == 0 = 0
    | null btns   = 0
    | otherwise   = search 1 (IntSet.singleton 0) [0]
  where
    -- BFS logic: explore states level by level (by number of button presses)
    search dist visited currentLayer
        | null currentLayer = 0 -- Should not be reachable for valid puzzle input
        | otherwise =
            let -- Generate all possible light states reachable in 'dist' presses
                nextStates = [ s `xor` b | s <- currentLayer, b <- btns ]
                -- Filter out states we have already seen to avoid cycles
                newStatesSet = IntSet.difference (IntSet.fromList nextStates) visited
            in if IntSet.member target newStatesSet
               then dist
               else if IntSet.null newStatesSet
                    then 0 -- No solution found for this machine
                    else search (dist + 1) (IntSet.union visited newStatesSet) (IntSet.toList newStatesSet)

main :: IO ()
main = do
    -- Open the input file and read its contents
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    
    -- Parse lines into machines, skipping any empty lines
    let machines = map parseLine (filter (not . null) (lines contents))
    
    -- Calculate the fewest button presses for each machine and sum them
    let totalPresses = sum $ map solveMachine machines
    
    -- Output the result to standard output
    print totalPresses
    
    hClose handle

