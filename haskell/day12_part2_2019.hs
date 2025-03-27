
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)
import Data.List (foldl', foldl1')
import GHC.Real (gcd) -- Use GHC's efficient gcd

type Point3 = (Int, Int, Int)
type SystemState = ([Point3], [Point3]) -- (Positions, Velocities)

-- Parsing
parseLine :: String -> Maybe Point3
parseLine s = case mapMaybe readMaybe numbers of
                  [x, y, z] -> Just (x, y, z)
                  _         -> Nothing
  where
    numbers = words $ map (\c -> if isDigit c || c == '-' then c else ' ') s

parseInput :: String -> SystemState
parseInput content = (positions, velocities)
  where
    positions = mapMaybe parseLine $ lines content
    velocities = replicate (length positions) (0, 0, 0)

-- Vector operations
addPoint3 :: Point3 -> Point3 -> Point3
addPoint3 (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

comparePoint3 :: Point3 -> Point3 -> Point3
comparePoint3 (x1, y1, z1) (x2, y2, z2) = (signum (x2 - x1), signum (y2 - y1), signum (z2 - z1))

-- Simulation Step
applyGravity :: SystemState -> SystemState
applyGravity (ps, vs) = (ps, zipWith addPoint3 vs deltaVs)
  where
    deltaVs = map (\p_i -> foldl' addPoint3 (0, 0, 0) [comparePoint3 p_i p_j | p_j <- ps]) ps

applyVelocity :: SystemState -> SystemState
applyVelocity (ps, vs) = (zipWith addPoint3 ps vs, vs)

step :: SystemState -> SystemState
step = applyVelocity . applyGravity

simulate :: Int -> SystemState -> SystemState
simulate n system = iterate step system !! n

-- Energy Calculation
pointEnergy :: Point3 -> Int
pointEnergy (x, y, z) = abs x + abs y + abs z

calculateEnergy :: SystemState -> Int
calculateEnergy (ps, vs) = sum $ zipWith (*) (map pointEnergy ps) (map pointEnergy vs)

-- Cycle Finding
getDim :: Int -> Point3 -> Int
getDim 0 (x, _, _) = x
getDim 1 (_, y, _) = y
getDim 2 (_, _, z) = z
getDim _ _ = error "Invalid dimension"

extractDim :: Int -> SystemState -> [(Int, Int)]
extractDim d (ps, vs) = zip (map (getDim d) ps) (map (getDim d) vs)

gravity1D :: [Int] -> [Int]
gravity1D ps1D = map (\p_i -> sum [signum (p_j - p_i) | p_j <- ps1D]) ps1D

step1D :: [(Int, Int)] -> [(Int, Int)]
step1D state1D = zipWith (\(p, v) dv -> (p + v + dv, v + dv)) state1D deltaVs
   where
     ps1D = map fst state1D
     deltaVs = gravity1D ps1D

findCycle :: [(Int, Int)] -> Int
findCycle initialState = go 1 (step1D initialState)
  where
    go !n currentState
      | currentState == initialState = n
      | otherwise = go (n + 1) (step1D currentState)

lcm' :: Int -> Int -> Int
lcm' a b
 | a == 0 || b == 0 = 0
 | otherwise = abs (a * (b `div` gcd a b))

findCycleLength :: SystemState -> Int
findCycleLength initialState = foldl1' lcm' cycles1D
  where
    states1D = map (`extractDim` initialState) [0, 1, 2]
    cycles1D = map findCycle states1D

-- Main
main :: IO ()
main = do
    input <- readFile "input.txt"
    let initialState = parseInput input

    let finalState1000 = simulate 1000 initialState
    let energy = calculateEnergy finalState1000
    putStrLn $ "Part 1 - Total energy after 1000 steps: " ++ show energy

    let cycleLen = findCycleLength initialState
    putStrLn $ "Part 2 - Number of steps to reach the first repeating state: " ++ show cycleLen

