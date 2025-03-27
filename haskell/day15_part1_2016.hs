
import Data.List (foldl')

data Disc = Disc { discId :: Int, totalPositions :: Int, startPosition :: Int } deriving Show

parseLine :: String -> Disc
parseLine line =
    let ws = words line
        dId = read $ init (ws !! 1) -- Remove '#'
        total = read (ws !! 3)
        start = read $ init (ws !! 11) -- Remove '.'
    in Disc dId total start

myGcd :: Int -> Int -> Int
myGcd a 0 = abs a
myGcd a b = myGcd b (a `mod` b)

myLcm :: Int -> Int -> Int
myLcm a b
    | a == 0 || b == 0 = 0
    | otherwise = abs ((a `div` myGcd a b) * b)

getCongruence :: (Int, Disc) -> (Int, Int)
getCongruence (idx, disc) =
    let modulus = totalPositions disc
        -- time + discId + startPosition = 0 (mod totalPositions) -- Original Python uses discId = i+1
        -- time + idx + 1 + startPosition = 0 (mod totalPositions)
        -- time = -(idx + 1 + startPosition) (mod totalPositions)
        requiredRemainder = (-(startPosition disc + idx + 1)) `mod` modulus
    in (requiredRemainder, modulus)

solveStep :: (Int, Int) -> (Int, Int) -> (Int, Int)
solveStep (currentTime, currentStep) (requiredRemainder, modulus) =
    let findTime t
            | t `mod` modulus == requiredRemainder = t
            | otherwise                          = findTime (t + currentStep)
        nextTime = findTime currentTime
        newStep = myLcm currentStep modulus
    in (nextTime, newStep)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let discs = map parseLine (lines contents)
    let indexedDiscs = zip [0..] discs
    let congruences = map getCongruence indexedDiscs
    let (solutionTime, _) = foldl' solveStep (0, 1) congruences
    print solutionTime

