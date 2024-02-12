
import Data.List

type Reindeer = (String, Int, Int, Int)

parseInput :: String -> [Reindeer]
parseInput input = map parseLine $ lines input
  where
    parseLine line = case words line of
      [name, _, _, speed, _, _, flyTime, _, _, _, _, _, _, restTime, _] -> (name, read speed, read flyTime, read restTime)
      _ -> error "Invalid input format"

distanceTraveled :: Reindeer -> Int -> Int
distanceTraveled (_, speed, flyTime, restTime) seconds = let
  cycleTime = flyTime + restTime
  fullCycles = seconds `div` cycleTime
  remainingTime = seconds `mod` cycleTime
  in (fullCycles * flyTime + min flyTime remainingTime) * speed

main = do
  input <- readFile "input.txt"
  let reindeers = parseInput input
  let distances = map (\reindeer -> distanceTraveled reindeer 2503) reindeers
  print $ maximum distances
