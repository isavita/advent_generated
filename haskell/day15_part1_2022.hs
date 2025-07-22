
import Data.List (foldl')
import Data.Char (isDigit)

type Point = (Int, Int)
data Sensor = Sensor { pos :: Point, beacon :: Point, dist :: Int } deriving Show

manhattan :: Point -> Point -> Int
manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

parseLine :: String -> Sensor
parseLine s = Sensor (sx,sy) (bx,by) (manhattan (sx,sy) (bx,by))
  where
    nums = map read $ words $ map (\c -> if isDigit c || c=='-' then c else ' ') s
    [sx,sy,bx,by] = nums

impossible :: [Sensor] -> Int -> Int
impossible sensors y = length $ filter id $ foldl' markBeacons (foldl' markCovered (replicate (maxX-minX+1) False) sensors) sensors
  where
    ranges = [(px - (d - abs (py - y)), px + (d - abs (py - y))) | Sensor (px,py) _ d <- sensors, abs (py - y) <= d]
    minX = minimum $ map fst ranges
    maxX = maximum $ map snd ranges
    markCovered arr (Sensor (px,py) _ d)
      | abs (py - y) > d = arr
      | otherwise = let start = px - (d - abs (py - y)) - minX
                        end = px + (d - abs (py - y)) - minX
                    in zipWith (\i b -> b || (i >= start && i <= end)) [0..] arr
    markBeacons arr (Sensor _ (bx,by) _)
      | by == y && bx >= minX && bx <= maxX = set (bx - minX) False arr
      | otherwise = arr
    set i v arr = take i arr ++ [v] ++ drop (i+1) arr

main :: IO ()
main = do
  sensors <- map parseLine . lines <$> readFile "input.txt"
  print $ impossible sensors 2000000
