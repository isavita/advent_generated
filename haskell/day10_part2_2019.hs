
import System.IO
import Data.List (sortBy)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

data Asteroid = Asteroid { x :: Int, y :: Int, angle :: Double, dist :: Double } deriving (Show)

readAsteroids :: FilePath -> IO [[Bool]]
readAsteroids filename = do
    contents <- readFile filename
    let lines' = lines contents
    return $ map (map (== '#')) lines'

vaporizeAsteroids :: [[Bool]] -> (Int, Int) -> [Asteroid]
vaporizeAsteroids asteroids (stationX, stationY) =
    let
        targets = [ Asteroid x y angle dist
                  | (y, row) <- zip [0..] asteroids
                  , (x, isAsteroid) <- zip [0..] row
                  , isAsteroid && not (x == stationX && y == stationY)
                  , let angle' = atan2 (fromIntegral (y - stationY)) (fromIntegral (x - stationX))
                        angle = if angle' < -pi/2 then angle' + 2*pi else angle'
                        dist = sqrt ((fromIntegral (x - stationX))^2 + (fromIntegral (y - stationY))^2)
                  ]
        sortedTargets = sortBy (\a b -> compare (angle a, dist a) (angle b, dist b)) targets
        vaporized = vaporize sortedTargets (-1000.0) []
    in
        vaporized
  where
    vaporize :: [Asteroid] -> Double -> [Asteroid] -> [Asteroid]
    vaporize [] _ acc = acc
    vaporize (a:rest) lastAngle acc =
      if angle a /= lastAngle
      then vaporize rest (angle a) (acc ++ [a])
      else vaporize rest lastAngle acc

findBestAsteroidLocation :: [[Bool]] -> ((Int, Int), Int)
findBestAsteroidLocation asteroids =
    maximumBy (\(_, count1) (_, count2) -> compare count1 count2)
    [ ((x, y), countVisibleAsteroids asteroids x y)
    | (y, row) <- zip [0..] asteroids
    , (x, isAsteroid) <- zip [0..] row
    , isAsteroid
    ]

maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy _ [] = error "maximumBy: empty list"
maximumBy cmp (x:xs) = foldl1 (\acc y -> if cmp acc y == LT then y else acc) (x:xs)

countVisibleAsteroids :: [[Bool]] -> Int -> Int -> Int
countVisibleAsteroids asteroids x y =
    Set.size $ Set.fromList
    [ atan2 (fromIntegral (otherY - y)) (fromIntegral (otherX - x))
    | (otherY, row) <- zip [0..] asteroids
    , (otherX, isAsteroid) <- zip [0..] row
    , isAsteroid && not (otherX == x && otherY == y)
    ]

main :: IO ()
main = do
    asteroids <- readAsteroids "input.txt"
    let (station, _) = findBestAsteroidLocation asteroids
    let vaporized = vaporizeAsteroids asteroids station
    if length vaporized >= 200
    then let target = vaporized !! 199 in print $ x target * 100 + y target
    else putStrLn "Less than 200 asteroids were vaporized."
