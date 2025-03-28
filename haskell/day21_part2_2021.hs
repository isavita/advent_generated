
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map.Strict as M
import System.IO (readFile)
import Text.Printf (printf)

-- Common utility for board movement
-- Position is 1-based, calculation uses 0-based then converts back
nextPos :: Int -> Int -> Int
nextPos currentPos rollSum = (currentPos + rollSum - 1) `mod` 10 + 1

-- --- Part 1: Deterministic Die ---

data GameStateP1 = GameStateP1
    { p1PosP1   :: !Int
    , p1ScoreP1 :: !Int
    , p2PosP1   :: !Int
    , p2ScoreP1 :: !Int
    , dieVal    :: !Int -- Next value the die will roll (1-100)
    , rolls     :: !Int -- Total number of rolls
    } deriving (Show)

-- Simulate one full turn (3 rolls for one player)
-- Returns Left score * rolls if game ends, otherwise Right newState
playTurnP1 :: GameStateP1 -> Either Int GameStateP1
playTurnP1 gs@GameStateP1{..} =
    -- Player 1's turn
    let (roll1, nextDie1) = rollDeterministic dieVal
        (roll2, nextDie2) = rollDeterministic nextDie1
        (roll3, nextDie3) = rollDeterministic nextDie2
        p1RollSum = roll1 + roll2 + roll3
        newP1Pos = nextPos p1PosP1 p1RollSum
        newP1Score = p1ScoreP1 + newP1Pos
        newRolls1 = rolls + 3
    in if newP1Score >= 1000
       then Left (p2ScoreP1 * newRolls1)
       else
           -- Player 2's turn
           let (roll4, nextDie4) = rollDeterministic nextDie3
               (roll5, nextDie5) = rollDeterministic nextDie4
               (roll6, nextDie6) = rollDeterministic nextDie5
               p2RollSum = roll4 + roll5 + roll6
               newP2Pos = nextPos p2PosP1 p2RollSum
               newP2Score = p2ScoreP1 + newP2Pos
               newRolls2 = newRolls1 + 3
           in if newP2Score >= 1000
              then Left (newP1Score * newRolls2)
              else Right $ GameStateP1 newP1Pos newP1Score newP2Pos newP2Score nextDie6 newRolls2
  where
    rollDeterministic :: Int -> (Int, Int) -- (rolled value, next die value)
    rollDeterministic currentVal = (currentVal, if currentVal == 100 then 1 else currentVal + 1)

solvePart1 :: Int -> Int -> Int
solvePart1 p1Start p2Start = loop initialState
  where
    initialState = GameStateP1 p1Start 0 p2Start 0 1 0
    loop :: GameStateP1 -> Int
    loop state = case playTurnP1 state of
        Left result   -> result
        Right newState -> loop newState

-- --- Part 2: Dirac Dice ---

data Player = P1 | P2 deriving (Eq, Ord, Show)
data StateP2 = StateP2
    { p1PosP2   :: !Int
    , p1ScoreP2 :: !Int
    , p2PosP2   :: !Int
    , p2ScoreP2 :: !Int
    , turn      :: !Player
    } deriving (Eq, Ord, Show)

type WinCounts = (Integer, Integer) -- (P1 wins, P2 wins)
type Memo = M.Map StateP2 WinCounts

-- Possible outcomes of 3 rolls of a 3-sided die and their frequencies
-- (Sum of 3 rolls, Frequency)
rollFreqs :: [(Int, Integer)]
rollFreqs = [(3,1), (4,3), (5,6), (6,7), (7,6), (8,3), (9,1)]

-- Recursive function with memoization to count wins
countWins :: StateP2 -> Memo -> (WinCounts, Memo)
countWins state memo =
    -- Check win condition *before* lookup/computation
    if p1ScoreP2 state >= 21 then ((1, 0), memo)
    else if p2ScoreP2 state >= 21 then ((0, 1), memo)
    else
        -- Check cache
        case M.lookup state memo of
            Just counts -> (counts, memo) -- Return cached result
            Nothing ->
                -- Compute recursively
                let (finalMemo, totalCounts) = foldl processRoll (memo, (0, 0)) rollFreqs
                -- Cache and return result
                in (totalCounts, M.insert state totalCounts finalMemo)
  where
    processRoll :: (Memo, WinCounts) -> (Int, Integer) -> (Memo, WinCounts)
    processRoll (currentMemo, (accW1, accW2)) (rollSum, freq) =
        let nextState = calcNextState state rollSum
            (recWins@(w1, w2), updatedMemo) = countWins nextState currentMemo
        -- Add the weighted wins from this branch
        in (updatedMemo, (accW1 + freq * w1, accW2 + freq * w2))

    calcNextState :: StateP2 -> Int -> StateP2
    calcNextState s@StateP2{..} rollSum =
      case turn of
        P1 -> let newPos = nextPos p1PosP2 rollSum
                  newScore = p1ScoreP2 + newPos
              in s { p1PosP2 = newPos, p1ScoreP2 = newScore, turn = P2 }
        P2 -> let newPos = nextPos p2PosP2 rollSum
                  newScore = p2ScoreP2 + newPos
              in s { p2PosP2 = newPos, p2ScoreP2 = newScore, turn = P1 }

solvePart2 :: Int -> Int -> Integer
solvePart2 p1Start p2Start = max w1 w2
  where
    initialState = StateP2 p1Start 0 p2Start 0 P1
    ( (w1, w2), _ ) = countWins initialState M.empty

-- --- Main Execution ---

parseStart :: String -> Int
parseStart line = read $ last $ words line -- Assumes "Player X starting position: Y"

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let ls = lines contents
    if length ls < 2
        then putStrLn "Error: input.txt should contain at least two lines for starting positions."
        else do
            let p1Start = parseStart (ls !! 0)
                p2Start = parseStart (ls !! 1)

            -- Calculate and Print Part 1 Result
            let result1 = solvePart1 p1Start p2Start
            printf "Part 1: %d\n" result1

            -- Calculate and Print Part 2 Result
            let result2 = solvePart2 p1Start p2Start
            printf "Part 2: %d\n" result2

