
{-# LANGUAGE OverloadedStrings #-} -- Optional, but can make string parsing cleaner

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (fromMaybe, listToMaybe, isJust)
import Data.List (sort)
import Control.Applicative ((<|>))
import System.IO (readFile)
import Text.Read (readMaybe) -- Safer reading

-- Data Types
type ChipValue = Int
type BotId = Int
type OutputId = Int

data Target = Bot BotId | Output OutputId
    deriving (Show, Eq, Ord)

data Instruction = InitValue ChipValue BotId
                 | GiveRule BotId Target Target
    deriving (Show)

-- State Representation
type BotState = Map BotId [ChipValue]      -- BotId -> Chips held (sorted list)
type OutputState = Map OutputId [ChipValue] -- OutputId -> Chips received
type Rules = Map BotId (Target, Target)     -- BotId -> (Low Target, High Target)

-- Overall simulation state including the answer for Part 1 once found
type SimState = (BotState, OutputState, Rules, Maybe BotId) -- (Bots, Outputs, Rules, Part1Answer)

-- Parsing Functions
parseTarget :: String -> String -> Maybe Target
parseTarget "bot" idStr = Bot <$> readMaybe idStr
parseTarget "output" idStr = Output <$> readMaybe idStr
parseTarget _ _ = Nothing

parseInstruction :: String -> Maybe Instruction
parseInstruction s = case words s of
    ["value", vStr, "goes", "to", "bot", bStr] ->
        InitValue <$> readMaybe vStr <*> readMaybe bStr
    ["bot", bStr, "gives", "low", "to", t1Type, t1Id, "and", "high", "to", t2Type, t2Id] -> do
        botId <- readMaybe bStr
        lowTarget <- parseTarget t1Type t1Id
        highTarget <- parseTarget t2Type t2Id
        Just $ GiveRule botId lowTarget highTarget
    _ -> Nothing

-- Initial State Construction
buildInitialState :: [Instruction] -> SimState
buildInitialState instructions = (initialBots, Map.empty, rules, Nothing)
  where
    initialBots = foldl addInitialValue Map.empty instructions
    rules = foldl addRule Map.empty instructions

    addInitialValue :: BotState -> Instruction -> BotState
    addInitialValue acc (InitValue v b) = Map.alter (Just . sort . (v:) . fromMaybe []) b acc
    addInitialValue acc _ = acc

    addRule :: Rules -> Instruction -> Rules
    addRule acc (GiveRule b tL tH) = Map.insert b (tL, tH) acc
    addRule acc _ = acc

-- Simulation Logic

-- Find a bot that has exactly two chips
findReadyBot :: BotState -> Maybe BotId
findReadyBot = listToMaybe . Map.keys . Map.filter ((== 2) . length)

-- Give a chip to a target (bot or output)
giveChip :: ChipValue -> Target -> (BotState, OutputState) -> (BotState, OutputState)
giveChip chip target (bots, outputs) = case target of
    Bot targetId ->
        -- Add chip and re-sort the bot's list
        (Map.alter (Just . sort . (chip:) . fromMaybe []) targetId bots, outputs)
    Output targetId ->
        -- Add chip to the output bin (order doesn't matter as much here, but let's keep it consistent)
        (bots, Map.alter (Just . (chip:) . fromMaybe []) targetId outputs)

-- Perform one step of the simulation
step :: SimState -> Maybe SimState -- Returns Nothing if no bot is ready
step state@(bots, outputs, rules, part1Answer) =
    case findReadyBot bots of
        Nothing -> Nothing -- Simulation stable
        Just readyBotId ->
            -- It's guaranteed by findReadyBot that the bot exists and has 2 chips
            let Just [lowChip, highChip] = Map.lookup readyBotId bots -- Chips are sorted
                Just (lowTarget, highTarget) = Map.lookup readyBotId rules -- Assume rule exists

                -- Check if this bot solves Part 1
                part1Found = if lowChip == 17 && highChip == 61 then Just readyBotId else Nothing
                -- Update Part 1 answer if found and not already set
                updatedPart1Answer = part1Answer <|> part1Found

                -- Bot gives away chips, its list becomes empty
                botsAfterGiving = Map.insert readyBotId [] bots

                -- Distribute chips to targets
                (botsAfterLow, outputsAfterLow) = giveChip lowChip lowTarget (botsAfterGiving, outputs)
                (finalBots, finalOutputs) = giveChip highChip highTarget (botsAfterLow, outputsAfterLow)

            in Just (finalBots, finalOutputs, rules, updatedPart1Answer)

-- Run the simulation until it stabilizes
runSimulation :: SimState -> SimState
runSimulation initialState = go initialState
  where
    go currentState =
        case step currentState of
            Nothing -> currentState -- Stable state reached
            Just nextState -> go nextState

-- Main Execution
main :: IO ()
main = do
    input <- readFile "input.txt"
    let maybeInstructions = map parseInstruction $ lines input

    -- Basic error checking for parsing
    if any null maybeInstructions then
        putStrLn "Error parsing input file."
    else do
        let instructions = [instr | Just instr <- maybeInstructions] -- Extract Just values
        let initialState = buildInitialState instructions
        let (finalBots, finalOutputs, _, finalPart1Answer) = runSimulation initialState

        -- Part 1 Output
        putStrLn $ "--- Day 10: Balance Bots ---"
        case finalPart1Answer of
            Just botId -> putStrLn $ "Part 1: Bot responsible for comparing 61 and 17 is " ++ show botId
            Nothing    -> putStrLn "Part 1: Bot responsible for comparing 61 and 17 not found."

        -- Part 2 Calculation and Output
        let getOutputValue oid = case Map.lookup oid finalOutputs of
                                   Just (chip:_) -> Just chip -- Assume first chip is the one needed
                                   _             -> Nothing
        
        let maybeOutput0 = getOutputValue 0
        let maybeOutput1 = getOutputValue 1
        let maybeOutput2 = getOutputValue 2

        case (*) <$> maybeOutput0 <*> ((*) <$> maybeOutput1 <*> maybeOutput2) of
             Just productVal -> putStrLn $ "Part 2: Product of outputs 0, 1, and 2 is " ++ show productVal
             Nothing         -> putStrLn "Part 2: Could not find values in outputs 0, 1, and 2."

