
import qualified Data.Map.Strict as M
import Data.Char (isDigit)
import Data.List (isInfixOf, foldl')
import System.IO (readFile)

-- Type definitions
type Tape = M.Map Int Int          -- Cursor position -> Value
type StateName = Char
type Move = Int                  -- -1 for left, +1 for right
type WriteValue = Int
type CurrentValue = Int
type Rule = (WriteValue, Move, StateName)
type StateRules = M.Map CurrentValue Rule -- Rules for a single state (0 -> rule, 1 -> rule)
type States = M.Map StateName StateRules -- All state definitions
type TuringState = (Tape, Int, StateName) -- (tape, cursor, currentState)

-- Parsing functions
parseBlueprint :: [String] -> (StateName, Int, States)
parseBlueprint ls = (initialState, steps, states)
  where
    initialState = last . init $ head ls
    steps = read . head . filter (all isDigit) . words $ ls !! 1
    states = parseStates (drop 2 ls)

parseStates :: [String] -> States
parseStates [] = M.empty
parseStates bloc
  | length bloc >= 9 = M.insert stateName rules (parseStates rest)
  | otherwise        = M.empty -- Or handle error for incomplete input
  where
    (currentStateBlock, rest) = splitAt 9 bloc
    stateName = last . init $ currentStateBlock !! 0

    parseRule :: [String] -> (CurrentValue, Rule)
    parseRule ruleLines = (val, (writeVal, moveDir, nextState))
      where
        val = read . pure . last . init $ ruleLines !! 0
        writeVal = read . pure . last . init $ ruleLines !! 1
        moveDir = if "left" `isInfixOf` (ruleLines !! 2) then -1 else 1
        nextState = last . init $ ruleLines !! 3

    (val0, rule0) = parseRule (drop 1 currentStateBlock)
    (val1, rule1) = parseRule (drop 5 currentStateBlock)
    rules = M.fromList [(val0, rule0), (val1, rule1)]

-- Simulation functions
runStep :: States -> TuringState -> TuringState
runStep states (tape, cursor, stateName) = (tape', cursor', nextState)
  where
    currentVal = M.findWithDefault 0 cursor tape
    stateRules = states M.! stateName -- Assumes state exists
    (writeValue, moveDir, nextState) = stateRules M.! currentVal -- Assumes rule exists
    tape' = M.insert cursor writeValue tape
    cursor' = cursor + moveDir

runSimulation :: Int -> States -> StateName -> Tape
runSimulation numSteps states initialState = finalTape
  where
    initialTuringState = (M.empty, 0, initialState)
    -- Use strict foldl' to avoid space leaks
    (finalTape, _, _) = foldl' (\acc _ -> runStep states acc) initialTuringState [1..numSteps]

calculateChecksum :: Tape -> Int
calculateChecksum = sum . M.elems

-- Main entry point
main :: IO ()
main = do
    contents <- readFile "input.txt"
    -- Filter empty lines and potentially strip whitespace/CR chars
    let fileLines = filter (not . null . filter (/= '\r')) . lines $ contents
    let (initialState, steps, states) = parseBlueprint fileLines
    let finalTape = runSimulation steps states initialState
    let checksum = calculateChecksum finalTape
    print checksum
