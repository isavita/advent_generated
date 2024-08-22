import Data.List (foldl')
import Data.Char (isDigit)
import System.IO (readFile)

data Vector = Vector { x :: Int, y :: Int, z :: Int } deriving (Show)
data Moon = Moon { position :: Vector, velocity :: Vector } deriving (Show)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let moons = map parseInput $ lines contents
    let finalMoons = simulateSteps moons 1000
    let totalEnergy = sum $ map calculateTotalEnergy finalMoons
    print totalEnergy

parseInput :: String -> Moon
parseInput line = Moon (Vector x y z) (Vector 0 0 0)
    where [x, y, z] = map read $ words $ map (\c -> if c == ',' || not (isDigit c) && c /= '-' then ' ' else c) line

simulateSteps :: [Moon] -> Int -> [Moon]
simulateSteps moons steps = foldl' (\ms _ -> updateMoons ms) moons [1..steps]

updateMoons :: [Moon] -> [Moon]
updateMoons moons = map updatePosition updatedVelocities
    where
        updatedVelocities = foldl' applyGravity moons [(i, j) | i <- [0..length moons - 1], j <- [0..length moons - 1], i /= j]
        applyGravity ms (i, j) = let m1 = ms !! i
                                     m2 = ms !! j
                                     newVel = updateVelocity (velocity m1) (position m1) (position m2)
                                 in replaceMoon ms i (m1 { velocity = newVel })
        updateVelocity vel pos1 pos2 = Vector (updateAxis (x pos1) (x pos2) (x vel)) (updateAxis (y pos1) (y pos2) (y vel)) (updateAxis (z pos1) (z pos2) (z vel))
        updateAxis p1 p2 v = if p1 < p2 then v + 1 else if p1 > p2 then v - 1 else v
        updatePosition moon = moon { position = Vector (x (position moon) + x (velocity moon)) (y (position moon) + y (velocity moon)) (z (position moon) + z (velocity moon)) }

replaceMoon :: [Moon] -> Int -> Moon -> [Moon]
replaceMoon moons idx newMoon = take idx moons ++ [newMoon] ++ drop (idx + 1) moons

calculateTotalEnergy :: Moon -> Int
calculateTotalEnergy moon = potentialEnergy * kineticEnergy
    where
        potentialEnergy = abs (x (position moon)) + abs (y (position moon)) + abs (z (position moon))
        kineticEnergy = abs (x (velocity moon)) + abs (y (velocity moon)) + abs (z (velocity moon))