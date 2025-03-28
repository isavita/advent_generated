
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.List (foldl', minimum, maximum)
import           System.IO (readFile)

-- Data Types
type Rules = Map (Char, Char) Char
type PairCounts = Map (Char, Char) Integer
type CharCounts = Map Char Integer

-- --- Parsing ---

-- Parses the input string into the template and the insertion rules.
parseInput :: String -> (String, Rules)
parseInput input =
    let (template : _ : ruleLines) = lines input
        rulesList = map parseRule ruleLines
    in (template, Map.fromList rulesList)
  where
    parseRule :: String -> ((Char, Char), Char)
    parseRule line =
        let [pairStr, "->", insertStr] = words line
        in case (pairStr, insertStr) of
             ([c1, c2], [insertChar]) -> ((c1, c2), insertChar)
             _ -> error $ "Invalid rule format: " ++ line

-- --- Core Logic ---

-- Calculates the initial counts of adjacent pairs in the template string.
initialPairCounts :: String -> PairCounts
initialPairCounts template = Map.fromListWith (+) $ map (, 1) $ zip template (tail template)

-- Performs one step of the pair insertion process based on counts.
-- Takes the rules and current pair counts, returns the next state of pair counts.
step :: Rules -> PairCounts -> PairCounts
step rules currentCounts = Map.fromListWith (+) $ concatMap expandPair $ Map.toList currentCounts
  where
    expandPair :: ((Char, Char), Integer) -> [((Char, Char), Integer)]
    expandPair (pair@(c1, c2), count) =
        case Map.lookup pair rules of
            -- If a rule exists (e.g., AB -> C), the pair AB generates AC and CB
            Just insertChar -> [ ((c1, insertChar), count)
                               , ((insertChar, c2), count)
                               ]
            -- If no rule exists for a pair, it theoretically shouldn't happen based on problem description
            -- but if it did, the pair would just persist (or disappear depending on interpretation).
            -- Assuming rules cover all generated pairs.
            Nothing -> error $ "Rule missing for pair: " ++ [c1, c2] -- Or handle as needed: `[ (pair, count) ]`

-- Applies the insertion step 'n' times.
applySteps :: Int -> Rules -> PairCounts -> PairCounts
applySteps n rules initialCounts = foldl' (\counts _ -> step rules counts) initialCounts [1..n]

-- Calculates the counts of individual characters from the final pair counts.
-- Crucially adds 1 to the count of the *last* character of the original template,
-- as it's the only character not accounted for when summing the first elements of pairs.
calculateCharCounts :: String -> PairCounts -> CharCounts
calculateCharCounts template finalPairCounts =
    let -- Sum counts based on the first character of each pair
        baseCounts = Map.fromListWith (+) $
                     map (\((c1, _), count) -> (c1, count)) $
                     Map.toList finalPairCounts
        -- Get the very last character of the original template
        lastChar = last template
    -- Add 1 to the count of the last character. Use insertWith to handle cases
    -- where the lastChar might not have appeared as the first element of any pair.
    in Map.insertWith (+) lastChar 1 baseCounts

-- --- Solving ---

-- Solves the puzzle for a given number of steps.
solve :: Int -> String -> Rules -> Integer
solve numSteps template rules =
    let initCounts = initialPairCounts template
        finalCounts = applySteps numSteps rules initCounts
        charCounts = calculateCharCounts template finalCounts
        counts = Map.elems charCounts
    in if null counts
       then 0
       else maximum counts - minimum counts

-- --- Main Entry Point ---

main :: IO ()
main = do
    -- Read input from the file
    input <- readFile "input.txt"

    -- Parse the input
    let (template, rules) = parseInput input

    -- Calculate and print the result for Part 1 (10 steps)
    let result10 = solve 10 template rules
    putStrLn $ "Part 1 (10 steps): " ++ show result10

    -- Calculate and print the result for Part 2 (40 steps)
    let result40 = solve 40 template rules
    putStrLn $ "Part 2 (40 steps): " ++ show result40
