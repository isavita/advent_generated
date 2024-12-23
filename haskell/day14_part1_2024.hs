
import Data.List
import Data.Char
import Control.Monad

parseLine :: String -> [Int]
parseLine line = let
    parts = words line
    pPart = drop 2 $ head parts
    vPart = drop 2 $ parts !! 1
    pos = map read $ split ',' pPart
    vel = map read $ split ',' vPart
  in pos ++ vel

split :: Char -> String -> [String]
split delimiter = foldr (\c acc -> if c == delimiter then "" : acc else (c : head acc) : tail acc) [""]

updateRobot :: Int -> Int -> [Int] -> [Int]
updateRobot width height r = let
    x = (r !! 0 + r !! 2) `mod` width
    y = (r !! 1 + r !! 3) `mod` height
  in [if x < 0 then x + width else x, if y < 0 then y + height else y, r !! 2, r !! 3]

simulate :: Int -> Int -> [[Int]] -> [[Int]]
simulate width height robots = iterate (map (updateRobot width height)) robots !! 100

countQuads :: [[Int]] -> (Int, Int, Int, Int)
countQuads robots = foldl' count (0,0,0,0) robots
  where
    count (q1,q2,q3,q4) r = let
        x = r !! 0
        y = r !! 1
      in if x == 50 || y == 51 then (q1,q2,q3,q4) else
        if x < 50 && y < 51 then (q1+1,q2,q3,q4) else
        if x > 50 && y < 51 then (q1,q2+1,q3,q4) else
        if x < 50 && y > 51 then (q1,q2,q3+1,q4) else
        (q1,q2,q3,q4+1)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let robots = map parseLine $ lines contents
  let updatedRobots = simulate 101 103 robots
  let (q1,q2,q3,q4) = countQuads updatedRobots
  print $ q1 * q2 * q3 * q4
