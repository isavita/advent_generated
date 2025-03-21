
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (liftM, liftM2, liftM3)
import Data.Maybe (catMaybes)

data Coord = Coord {x :: Int, y :: Int, z :: Int} deriving (Show)

data Point = Point {pos :: Coord, vel :: Coord} deriving (Show)

parseInput :: String -> [Point]
parseInput input = map parseLine $ lines input
  where
    parseLine line =
      let [posPart, velPart] = splitOn " @ " line
          [posX, posY, posZ] = map read $ splitOn ", " posPart
          [velX, velY, velZ] = map read $ splitOn ", " velPart
          pos = Coord posX posY posZ
          vel = Coord velX velY velZ
       in Point pos vel

splitOn :: String -> String -> [String]
splitOn delim str = splitOn' delim str []
  where
    splitOn' _ "" acc = reverse (("" : acc))
    splitOn' delim str acc =
      case breakOn delim str of
        (before, "") -> reverse (before : acc)
        (before, after) -> splitOn' delim (drop (length delim) after) (before : acc)

breakOn :: String -> String -> (String, String)
breakOn delim str =
  case findSequence delim str of
    Nothing -> (str, "")
    Just idx -> splitAt idx str

findSequence :: String -> String -> Maybe Int
findSequence needle haystack = findSequence' needle haystack 0
  where
    findSequence' _ "" _ = Nothing
    findSequence' "" _ _ = Nothing
    findSequence' needle haystack idx =
      if take (length needle) haystack == needle
        then Just idx
        else findSequence' needle (drop 1 haystack) (idx + 1)

isIntersecting2D :: Point -> Point -> Maybe (Coord, Double, Double)
isIntersecting2D p1 p2 =
  let det = fromIntegral (x (vel p1) * y (vel p2) - x (vel p2) * y (vel p1))
   in if det == 0
        then Nothing
        else
          let t1 = (fromIntegral (y (vel p2) * (x (pos p2) - x (pos p1)) - x (vel p2) * (y (pos p2) - y (pos p1)))) / det
              t2 = (fromIntegral (y (vel p1) * (x (pos p2) - x (pos p1)) - x (vel p1) * (y (pos p2) - y (pos p1)))) / det
              coordX = fromIntegral (x (pos p1)) + fromIntegral (x (vel p1)) * t1
              coordY = fromIntegral (y (pos p1)) + fromIntegral (y (vel p1)) * t1
           in Just (Coord (floor coordX) (floor coordY) 0, t1, t2)

solve :: [Point] -> Int -> Int -> Int
solve points minVal maxVal =
  length $
    filter
      ( \(coord, t1, t2) ->
          let coordX = fromIntegral $ x coord
              coordY = fromIntegral $ y coord
           in fromIntegral minVal <= coordX
                && coordX <= fromIntegral maxVal
                && fromIntegral minVal <= coordY
                && coordY <= fromIntegral maxVal
                && t1 >= 0
                && t2 >= 0
      )
      intersections
  where
    pointPairs = [(points !! i, points !! j) | i <- [0 .. length points - 1], j <- [0 .. i - 1]]
    intersections = catMaybes $ map (\(p1, p2) -> case isIntersecting2D p1 p2 of
                                                      Just (coord, t1, t2) -> Just (coord, t1, t2)
                                                      Nothing -> Nothing) pointPairs

main :: IO ()
main = do
  input <- readFile "input.txt"
  let points = parseInput input
  print $ solve points 200000000000000 400000000000000
