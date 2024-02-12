
import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Map as Map

type Point = (Int, Int)

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

closestPoint :: [Point] -> Point -> Maybe Point
closestPoint points p = case sortBy (compare `on` manhattanDistance p) points of
    [x] -> Just x
    x:y:_ | manhattanDistance p x == manhattanDistance p y -> Nothing
    x:_ -> Just x

getCoordinates :: String -> Point
getCoordinates s = let [x, y] = map read $ words $ map (\c -> if c == ',' then ' ' else c) s in (x, y)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let coordinates = map getCoordinates $ lines contents
        maxX = maximum $ map fst coordinates
        maxY = maximum $ map snd coordinates
        grid = [(x, y) | x <- [0..maxX], y <- [0..maxY]]
        closests = mapMaybe (closestPoint coordinates) grid
        infiniteAreas = nub $ map fromJust $ filter isJust $ map (closestPoint coordinates) $ filter (\(x, y) -> x == 0 || y == 0 || x == maxX || y == maxY) grid
        areas = Map.toList $ Map.fromListWith (+) $ map (\p -> (p, 1)) closests
        validAreas = filter (\(p, _) -> p `notElem` infiniteAreas) areas
        result = maximum $ map snd validAreas
    print result
