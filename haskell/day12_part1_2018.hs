
{-# LANGUAGE ViewPatterns #-}

import           Data.Map (Map, fromList, (!))
import qualified Data.Map as M
import           Data.Set (Set, fromList)
import qualified Data.Set as S
import           Data.Maybe (mapMaybe)

type Rule = (Bool, Bool, Bool, Bool, Bool)
type Rules = Map Rule Bool
type State = Set Int

-- | Parses the input file into the initial state and the rules.
parseInput :: String -> (State, Rules)
parseInput input = (initialState, rules)
  where
    lines_ = lines input
    initialStateStr = drop (length "initial state: ") . head $ lines_
    initialState = S.fromList [i | (c, i) <- zip initialStateStr [0..], c == '#']
    ruleLines = drop 2 lines_  -- Skip the blank line and initial state.
    rules = M.fromList $ map parseRule ruleLines

-- | Parses a single rule line.
parseRule :: String -> (Rule, Bool)
parseRule line = (toRule $ take 5 line, last line == '#')
  where
    toRule [a, b, c, d, e] = (c2b a, c2b b, c2b c, c2b d, c2b e)
    c2b '#' = True
    c2b _   = False
    
-- | Converts a list of booleans (representing a rule) into a Rule.
ruleToList :: Rule -> [Bool]
ruleToList (a, b, c, d, e) = [a, b, c, d, e]


-- | Applies the rules to a single pot.
applyRule :: Rules -> State -> Int -> Bool
applyRule rules state i = rules ! rule
  where
    rule = (
        S.member (i - 2) state,
        S.member (i - 1) state,
        S.member i state,
        S.member (i + 1) state,
        S.member (i + 2) state
        )

-- | Performs a single generation step.
nextGeneration :: Rules -> State -> State
nextGeneration rules state =
  let minPot = S.findMin state - 2
      maxPot = S.findMax state + 2
      potsToCheck = [minPot..maxPot]
  in S.fromList [pot | pot <- potsToCheck, applyRule rules state pot]

-- | Calculates the sum of the pot numbers containing plants.
calculateSum :: State -> Int
calculateSum = S.foldl (+) 0

-- | Simulates the plant growth for a given number of generations.
simulate :: Int -> State -> Rules -> State
simulate 0 state _ = state
simulate n state rules = simulate (n - 1) (nextGeneration rules state) rules

-- | Main function. Reads input, simulates, and prints the result.
main :: IO ()
main = do
  input <- readFile "input.txt"
  let (initialState, rules) = parseInput input
  let finalState = simulate 20 initialState rules
  let result = calculateSum finalState
  print result
