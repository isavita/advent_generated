
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import System.IO (readFile)
import Control.Monad (foldM) -- Using foldM for iterative state updates
import Data.List (foldl')
import Data.Maybe (fromMaybe)

-- Represents the state of the game at any point.
-- Using strictness annotations (!) to prevent space leaks with large inputs.
data GameState = GameState {
    circle       :: !(Seq Int),      -- The circle of marbles, using Sequence for efficient insertion/deletion
    currentIndex :: !Int,          -- Index of the current marble in the sequence
    scores       :: !(IntMap Int),  -- Scores for each player (Player Index -> Score)
    nextMarble   :: !Int           -- The value of the next marble to be placed
} deriving (Show)

-- Parses the input string "P players; last marble is worth L points"
parseInput :: String -> (Int, Int)
parseInput s =
    let ws = words s
        players = read (ws !! 0)
        lastMarble = read (ws !! 6)
    in (players, lastMarble)

-- Performs a single turn of the game
playTurn :: Int -> Int -> GameState -> GameState
playTurn numPlayers lastMarbleValue gs@GameState{..} =
    let !marbleValue = nextMarble
    in if marbleValue > lastMarbleValue
       then gs -- Game ends
       else
         if marbleValue `rem` 23 == 0
         then -- Special turn (multiple of 23)
           let !circleSize = Seq.length circle
               -- Index of the marble to remove (7 counter-clockwise)
               -- Modulo arithmetic needs care: (a - b) mod n == (a - b + n) mod n
               !removeIndex = (currentIndex - 7 + circleSize) `mod` circleSize
               -- Get the value of the marble being removed
               !removedMarbleValue = Seq.index circle removeIndex
               -- Calculate the current player (0-indexed)
               !currentPlayer = (marbleValue - 1) `mod` numPlayers
               -- Calculate the score increase for this turn
               !scoreIncrease = marbleValue + removedMarbleValue
               -- Update the scores map
               !newScores = IntMap.adjust (+ scoreIncrease) currentPlayer scores
               -- Remove the marble from the circle
               !newCircle = Seq.deleteAt removeIndex circle
               -- The new current marble is the one immediately clockwise to the removed one.
               -- Since deleteAt shifts elements left, the new current index is removeIndex.
               -- We need modulo the *new* size in case the last element was removed.
               !newCurrentIndex = removeIndex `mod` Seq.length newCircle

           in GameState {
                 circle = newCircle,
                 currentIndex = newCurrentIndex,
                 scores = newScores,
                 nextMarble = marbleValue + 1
               }
         else -- Normal turn
           let !circleSize = Seq.length circle
               -- Calculate insertion index: 2 positions clockwise from current
               -- insertAt inserts *at* the index, shifting subsequent elements
               -- We want to insert between index `(currentIndex + 1)` and `(currentIndex + 2)`.
               -- So, the insertion index should be `(currentIndex + 2) mod circleSize`.
               -- Handle the edge case of inserting at the very end (index == size) correctly.
               !insertIndex' = (currentIndex + 1) `mod` circleSize
               !insertIndex  = insertIndex' + 1 -- insert *after* the marble at index `insertIndex'`
               -- Insert the new marble
               !newCircle = Seq.insertAt insertIndex marbleValue circle
               -- The newly inserted marble becomes the current marble
               !newCurrentIndex = insertIndex

           in GameState {
                 circle = newCircle,
                 currentIndex = newCurrentIndex,
                 scores = scores, -- Scores don't change on a normal turn
                 nextMarble = marbleValue + 1
               }

-- Runs the game simulation from start to finish
runGame :: Int -> Int -> GameState
runGame numPlayers lastMarbleValue =
    -- Initial state: Marble 0 is placed, current index is 0, next marble is 1.
    let initialState = GameState {
            circle = Seq.singleton 0,
            currentIndex = 0,
            scores = IntMap.fromList $ zip [0..numPlayers-1] (repeat 0),
            nextMarble = 1
        }
        -- Fold playTurn over the range of marbles [1..lastMarbleValue]
        -- Using foldl' for strictness
    in foldl' (\state _ -> playTurn numPlayers lastMarbleValue state) initialState [1..lastMarbleValue]

-- Solves the puzzle for a given number of players and last marble value
solve :: Int -> Int -> Int
solve numPlayers lastMarbleValue
    | numPlayers <= 0 || lastMarbleValue < 0 = 0 -- Handle invalid input gracefully
    | lastMarbleValue == 0 = 0 -- No turns played if last marble is 0
    | otherwise =
        let finalState = runGame numPlayers lastMarbleValue
            finalScores = IntMap.elems (scores finalState)
        -- Find the maximum score, defaulting to 0 if no scores exist (shouldn't happen with valid input)
        in fromMaybe 0 $ foldl' maxMaybe Nothing finalScores
           where maxMaybe Nothing y = Just y
                 maxMaybe (Just x) y = Just (max x y)


-- Main entry point
main :: IO ()
main = do
    -- Read input from the file
    contents <- readFile "input.txt"
    let (players, lastMarble) = parseInput contents

    -- Calculate and print the result for Part 1
    let result1 = solve players lastMarble
    print result1

    -- Calculate and print the result for Part 2 (last marble * 100)
    let result2 = solve players (lastMarble * 100)
    print result2

