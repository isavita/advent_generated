
{-# LANGUAGE TupleSections #-}

import Data.List (minimumBy, transpose)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String (Parser)

data Point = Point { x :: Int, y :: Int, vx :: Int, vy :: Int } deriving (Show, Eq)

-- Parsing the input
pointParser :: Parser Point
pointParser = do
  string "position=<"
  spaces
  x <- read <$> many1 (oneOf "-0123456789")
  string ","
  spaces
  y <- read <$> many1 (oneOf "-0123456789")
  string "> velocity=<"
  spaces
  vx <- read <$> many1 (oneOf "-0123456789")
  string ","
  spaces
  vy <- read <$> many1 (oneOf "-0123456789")
  string ">"
  return $ Point x y vx vy

parseInput :: String -> [Point]
parseInput = either (error . show) id . traverse (parse pointParser "") . lines

-- Simulate a single step
step :: [Point] -> [Point]
step = map (\(Point x y vx vy) -> Point (x + vx) (y + vy) vx vy)

-- Calculate the bounding box area
boundingBoxArea :: [Point] -> Int
boundingBoxArea points =
  let minX = minimum $ map x points
      maxX = maximum $ map x points
      minY = minimum $ map y points
      maxY = maximum $ map y points
  in (maxX - minX + 1) * (maxY - minY + 1)

-- Find the time step with the smallest bounding box area (most compact)
findMinAreaTime :: [Point] -> Int
findMinAreaTime initialPoints = go initialPoints 0
  where
    go points time =
      let nextPoints = step points
          currentArea = boundingBoxArea points
          nextArea = boundingBoxArea nextPoints
      in if nextArea > currentArea
         then time  -- Area is increasing, previous step was the minimum
         else go nextPoints (time + 1)

-- Advance points to a specific time
advancePoints :: [Point] -> Int -> [Point]
advancePoints points time = iterate step points !! time

-- Convert points to a 2D grid for printing, handling edge cases gracefully.
pointsToGrid :: [Point] -> [String]
pointsToGrid points =
    if null points then [] else
      let minX = minimum $ map x points
          maxX = maximum $ map x points
          minY = minimum $ map y points
          maxY = maximum $ map y points
          width = maxX - minX + 1
          height = maxY - minY + 1
          pointMap = M.fromList $ map (\p -> ((x p, y p), '#')) points
          getRow y = map (\x -> M.findWithDefault '.' (x, y) pointMap) [minX..maxX]
      in map getRow [minY..maxY]



-- Main function
main :: IO ()
main = do
  contents <- readFile "input.txt"
  let points = parseInput contents
  let minTime = findMinAreaTime points
  let alignedPoints = advancePoints points minTime
  let grid = pointsToGrid alignedPoints
  mapM_ putStrLn grid
