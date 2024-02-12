
import Data.List
import Data.Maybe
import System.IO

main = do
    contents <- readFile "input.txt"
    let asteroids = map (map (== '#')) $ lines contents
        maxCount = findBestAsteroidLocation asteroids
    print maxCount

findBestAsteroidLocation :: [[Bool]] -> Int
findBestAsteroidLocation asteroids = maximum [countVisibleAsteroids asteroids x y | (y, row) <- zip [0..] asteroids, (x, isAsteroid) <- zip [0..] row, isAsteroid]

countVisibleAsteroids :: [[Bool]] -> Int -> Int -> Int
countVisibleAsteroids asteroids x y = length $ nub [angle (otherX - x) (otherY - y) | (otherY, row) <- zip [0..] asteroids, (otherX, isAsteroid) <- zip [0..] row, isAsteroid, (otherX, otherY) /= (x, y)]
    where angle dx dy = atan2 (fromIntegral dy) (fromIntegral dx)
