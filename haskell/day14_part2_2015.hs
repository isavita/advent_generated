import System.IO
import Data.List
import Data.Maybe

data Reindeer = Reindeer { name :: String, speed :: Int, flyTime :: Int, restTime :: Int } deriving (Show)

parseInput :: String -> [Reindeer]
parseInput = map parseLine . lines
  where
    parseLine line = let parts = words line
                         name = head parts
                         speed = read (parts !! 3)
                         flyTime = read (parts !! 6)
                         restTime = read (parts !! 13)
                     in Reindeer name speed flyTime restTime

distanceAfterSeconds :: Reindeer -> Int -> Int
distanceAfterSeconds reindeer seconds = let cycleTime = flyTime reindeer + restTime reindeer
                                            fullCycles = seconds `div` cycleTime
                                            remainingTime = seconds `mod` cycleTime
                                            flyDuration = if remainingTime > flyTime reindeer then flyTime reindeer else remainingTime
                                        in (fullCycles * flyTime reindeer + flyDuration) * speed reindeer

pointsAfterSeconds :: [Reindeer] -> Int -> [Int]
pointsAfterSeconds reindeers totalSeconds = let distances = replicate (length reindeers) 0
                                                points = replicate (length reindeers) 0
                                                updatePoints distances points = let maxDistance = maximum distances
                                                                                in map (\(d, p) -> if d == maxDistance then p + 1 else p) (zip distances points)
                                            in foldl (\points second -> let distances = map (\r -> distanceAfterSeconds r second) reindeers
                                                                        in updatePoints distances points) points [1..totalSeconds]

main = do
    contents <- readFile "input.txt"
    let reindeers = parseInput contents
        points = pointsAfterSeconds reindeers 2503
        maxPoints = maximum points
    print maxPoints