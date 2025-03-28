
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (foldl', findIndex, findIndices)
import System.IO (readFile)

-- Data types
type PotState = Char -- '#' or '.'
type Pots = (Integer, String) -- (index of first pot, pots string)
type Rules = Set.Set String -- Set of 5-char patterns that produce a plant ('#')

-- Constants
paddingSize :: Int
paddingSize = 4 -- Should be enough to cover rule lookahead (2) + potential growth (2)

paddingString :: String
paddingString = replicate paddingSize '.'

targetGenerations1 :: Integer
targetGenerations1 = 20

targetGenerations2 :: Integer
targetGenerations2 = 50000000000

-- Parsing
parseInput :: String -> (Pots, Rules)
parseInput input = (initialPots, rules)
  where
    ls = lines input
    initialStateLine = head ls
    rulesLines = drop 2 ls

    initialStr = drop (length "initial state: ") initialStateLine
    initialPots = (0, initialStr) -- Starts at index 0

    rules = Set.fromList $ map (take 5) $ filter (\line -> last line == '#') rulesLines

-- Simulation Step
step :: Rules -> Pots -> Pots
step rules (startIndex, currentPots) = (newStartIndex, newPotsString)
  where
    -- Pad the current pots string
    paddedPots = paddingString ++ currentPots ++ paddingString
    paddedStartIndex = startIndex - fromIntegral paddingSize

    -- Generate the next generation string based on rules
    nextPaddedPots = map applyRule [0 .. length paddedPots - 1]
      where
        applyRule i
          | i < 2 || i >= length paddedPots - 2 = '.' -- Edge pots outside rule range become empty
          | otherwise =
              let pattern = take 5 $ drop (i - 2) paddedPots
              in if Set.member pattern rules then '#' else '.'

    -- Trim leading/trailing '.' and adjust start index
    firstPlantIdx = findIndex (== '#') nextPaddedPots
    lastPlantIdx = fmap (\idx -> length nextPaddedPots - 1 - idx) $ findIndex (== '#') (reverse nextPaddedPots)

    (newStartIndex, newPotsString) = case (firstPlantIdx, lastPlantIdx) of
        (Just first, Just last) ->
            let trimmedPots = take (last - first + 1) $ drop first nextPaddedPots
                newIdx = paddedStartIndex + fromIntegral first
            in (newIdx, trimmedPots)
        _ -> (0, "") -- No plants left

-- Calculate sum of pot indices containing plants
calculateSum :: Pots -> Integer
calculateSum (startIndex, pots) = sum $ map (\i -> startIndex + fromIntegral i) $ findIndices (== '#') pots

-- Simulation with cycle detection for Part 2
simulateGenerations :: Integer -> Rules -> Pots -> Integer
simulateGenerations targetGen rules initialPots = go 0 Map.empty initialPots
  where
    go :: Integer -> Map.Map String (Integer, Integer) -> Pots -> Integer
    go currentGen seen (currentIdx, currentStr)
      | currentGen == targetGen = calculateSum (currentIdx, currentStr)
      | otherwise =
          case Map.lookup currentStr seen of
            Just (prevGen, prevIdx) ->
              -- Cycle detected!
              let
                cycleLen = currentGen - prevGen
                idxDiff = currentIdx - prevIdx
                remainingGens = targetGen - currentGen
                -- The pattern repeats every cycleLen generations, shifting by idxDiff each time.
                -- Calculate the final index after remainingGens.
                finalIdx = currentIdx + (remainingGens `div` cycleLen) * idxDiff
                -- The number of steps within the *last* partial cycle
                remainingStepsInCycle = remainingGens `mod` cycleLen
                -- To get the exact final state, we'd need to step remainingStepsInCycle times
                -- from the current state. However, the *sum* calculation simplifies.
                -- Each step within the cycle shifts the sum by a fixed amount (related to idxDiff and number of plants).
                -- Let's calculate the final index based on the stable shift per generation.
                shiftPerGen = idxDiff `div` cycleLen -- Assumes stable shift
                finalIdxStableShift = currentIdx + remainingGens * shiftPerGen
                -- This assumes the shift *per generation* becomes constant once the pattern stabilizes.
              in
                -- Since the *pattern* `currentStr` is the same, we just need the final index.
                calculateSum (finalIdxStableShift, currentStr)

            Nothing ->
              -- No cycle detected yet, continue simulation
              let nextPots = step rules (currentIdx, currentStr)
                  newSeen = Map.insert currentStr (currentGen, currentIdx) seen
              in go (currentGen + 1) newSeen nextPots

-- Main function
main :: IO ()
main = do
    input <- readFile "input.txt"
    let (initialPots, rules) = parseInput input

    -- Part 1: Simulate 20 generations
    let potsAfter20Gens = foldl' (\p _ -> step rules p) initialPots [1..targetGenerations1]
    let sumPart1 = calculateSum potsAfter20Gens
    putStrLn $ "Part 1: " ++ show sumPart1

    -- Part 2: Simulate 50 billion generations with cycle detection
    let sumPart2 = simulateGenerations targetGenerations2 rules initialPots
    putStrLn $ "Part 2: " ++ show sumPart2

