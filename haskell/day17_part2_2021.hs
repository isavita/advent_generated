
import System.IO
import Text.Read (readMaybe)
import Control.Monad (guard)
import Data.List (isPrefixOf, break, head, lines, dropWhile)

parseInput :: String -> Maybe (Int, Int, Int, Int)
parseInput s = do
    guard ("target area: x=" `isPrefixOf` s)
    let s1 = drop (length "target area: x=") s
    let (xPart, s2) = break (== ',') s1
    guard (not (null s2) && ", y=" `isPrefixOf` s2)
    let yPart = drop (length ", y=") s2

    let (xMinStr, xMaxStr') = break (== '.') xPart
    guard (not (null xMaxStr') && ".." `isPrefixOf` xMaxStr')
    let xMaxStr = drop 2 xMaxStr'
    xMin <- readMaybe xMinStr
    xMax <- readMaybe xMaxStr

    let (yMinStr, yMaxStr') = break (== '.') yPart
    guard (not (null yMaxStr') && ".." `isPrefixOf` yMaxStr')
    let yMaxStr = drop 2 yMaxStr'
    yMin <- readMaybe yMinStr
    yMax <- readMaybe yMaxStr

    return (xMin, xMax, yMin, yMax)

hitsTarget :: (Int, Int, Int, Int) -> (Int, Int) -> Bool
hitsTarget (xMin, xMax, yMin, yMax) (vx0, vy0) = simulate 0 0 vx0 vy0
  where
    simulate x y vx vy
      | x >= xMin && x <= xMax && y >= yMin && y <= yMax = True
      | x > xMax && vx >= 0 = False
      | y < yMin && vy < 0  = False
      | vx == 0 && (x < xMin || x > xMax) = False
      | otherwise = simulate nextX nextY nextVx nextVy
      where
        nextX = x + vx
        nextY = y + vy
        nextVx | vx > 0 = vx - 1
               | vx < 0 = vx + 1
               | otherwise = 0
        nextVy = vy - 1

findMinVx :: Int -> Int
findMinVx xMinTarget
    | xMinTarget <= 0 = 0
    | otherwise = head $ dropWhile (\v -> v * (v + 1) < 2 * xMinTarget) [0..]

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let firstLine = head $ lines contents
    case parseInput firstLine of
        Nothing -> putStrLn "Error parsing input"
        Just target@(xMin, xMax, yMin, yMax) -> do
            let minVx = findMinVx xMin
            let maxVx = xMax
            let minVy = yMin
            -- If vy > 0, it returns to y=0 with speed -(vy+1).
            -- Must not overshoot yMin in the next step: -(vy+1) >= yMin => vy <= -yMin - 1.
            -- Use abs yMin as a simpler, slightly larger safe upper bound common in solutions.
            let maxVy = if yMin < 0 then abs yMin else max 100 (abs yMax) -- Heuristic if yMin >= 0

            let vxRange = [minVx .. maxVx]
            let vyRange = [minVy .. maxVy]

            let velocities = [(vx, vy) | vx <- vxRange, vy <- vyRange]
            let validCount = length $ filter (hitsTarget target) velocities
            print validCount

