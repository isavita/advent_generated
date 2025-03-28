
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative ((<|>))
import Data.List (foldl')
import Text.ParserCombinators.ReadP
import qualified Data.Maybe as Maybe

-- Data Types

-- Represents an inclusive range [lo, hi]
type Range = (Integer, Integer)

-- Represents a 3D cuboid with ranges for x, y, and z coordinates
data Cuboid = Cuboid { xRange :: !Range, yRange :: !Range, zRange :: !Range }
    deriving (Show, Eq, Ord)

-- Represents a reboot step: True for 'on', False for 'off', and the affected Cuboid
type Step = (Bool, Cuboid)

-- Parsing

-- Parses an integer (possibly negative)
integer :: ReadP Integer
integer = readS_to_P reads

-- Parses a range like "10..12"
parseRange :: ReadP Range
parseRange = do
    lo <- integer
    _ <- string ".."
    hi <- integer
    return (lo, hi)

-- Parses a coordinate range like "x=10..12"
parseCoord :: Char -> ReadP Range
parseCoord c = do
    _ <- char c
    _ <- char '='
    parseRange

-- Parses a full instruction line
parseLine :: ReadP Step
parseLine = do
    stateStr <- string "on" <|> string "off"
    _ <- char ' '
    xR <- parseCoord 'x'
    _ <- char ','
    yR <- parseCoord 'y'
    _ <- char ','
    zR <- parseCoord 'z'
    eof -- Ensure the whole line is consumed
    let state = stateStr == "on"
    return (state, Cuboid xR yR zR)

-- Function to run the ReadP parser on a single line
runParser :: String -> Maybe Step
runParser s = case readP_to_S parseLine s of
    [(res, "")] -> Just res -- Success: unique complete parse
    _           -> Nothing  -- Failed or ambiguous parse

-- Core Logic: Inclusion-Exclusion

-- Calculate the volume of a cuboid. Returns 0 for invalid ranges (lo > hi).
volume :: Cuboid -> Integer
volume Cuboid{..} = dim xRange * dim yRange * dim zRange
  where
    dim :: Range -> Integer
    dim (lo, hi) = max 0 (hi - lo + 1)

-- Calculate the intersection of two ranges.
intersectRange :: Range -> Range -> Maybe Range
intersectRange (lo1, hi1) (lo2, hi2) =
    let lo = max lo1 lo2
        hi = min hi1 hi2
    in if lo <= hi then Just (lo, hi) else Nothing

-- Calculate the intersection of two cuboids.
intersection :: Cuboid -> Cuboid -> Maybe Cuboid
intersection c1 c2 = do -- Using Maybe monad
    xR <- intersectRange (xRange c1) (xRange c2)
    yR <- intersectRange (yRange c1) (yRange c2)
    zR <- intersectRange (zRange c1) (zRange c2)
    return $ Cuboid xR yR zR

-- Process a single reboot step using the inclusion-exclusion principle.
-- Accumulator `acc` is a list of (Cuboid, Sign), where Sign is +1 or -1.
-- A new step (state, newCuboid) interacts with existing cuboids in the accumulator.
processStep :: [(Cuboid, Integer)] -> Step -> [(Cuboid, Integer)]
processStep acc (state, newCuboid) =
    -- For each existing cuboid (c, sign) in acc, find its intersection with newCuboid.
    -- If they intersect, create a new cuboid representing the intersection,
    -- but with the *opposite* sign (-sign). This corrects for double counting
    -- (if adding an 'on' region) or incorrectly removed volume (if adding an 'off' region).
    let !corrections = Maybe.mapMaybe (\(c, sign) -> intersection c newCuboid >>= \i -> Just (i, -sign)) acc
        -- If the current step is 'on', add the new cuboid itself with a +1 sign.
        !newState = if state then (newCuboid, 1) : corrections else corrections
    in acc ++ newState -- Append the new state changes (original cuboid if 'on', and all corrections)

-- Apply clipping to a cuboid
clipCuboid :: Cuboid -> Cuboid -> Maybe Cuboid
clipCuboid = intersection

-- Main Execution

-- Solve the problem for a list of steps, optionally clipping to a region.
solve :: Maybe Cuboid -> [Step] -> Integer
solve mClippingRegion steps =
    let relevantSteps = case mClippingRegion of
            Nothing -> steps -- Part 2: Use all steps as is
            Just clipRegion -> Maybe.mapMaybe (applyClip clipRegion) steps -- Part 1: Clip each step's cuboid

        -- Apply the clipping region to a single step
        applyClip clipRegion (state, cuboid) =
            clipCuboid clipRegion cuboid >>= \clipped -> Just (state, clipped)

        -- Run the inclusion-exclusion process
        finalSignedCuboids = foldl' processStep [] relevantSteps

        -- Sum the signed volumes
        totalVolume = sum [volume c * sign | (c, sign) <- finalSignedCuboids]
    in totalVolume

main :: IO ()
main = do
    input <- readFile "input.txt"
    let steps = Maybe.mapMaybe runParser (lines input)

    -- Define the initialization region for Part 1
    let initRegion = Cuboid (-50, 50) (-50, 50) (-50, 50)

    -- Calculate and print result for Part 1
    let resultPart1 = solve (Just initRegion) steps
    putStrLn $ "Part 1: " ++ show resultPart1

    -- Calculate and print result for Part 2
    let resultPart2 = solve Nothing steps
    putStrLn $ "Part 2: " ++ show resultPart2
