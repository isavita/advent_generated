
import Data.List
import Data.Maybe

parseInput :: String -> [(String, String, Int)]
parseInput = map parseLine . lines
  where
    parseLine line = let [src, _, dest, _, dist] = words line
                     in (src, dest, read dist)

getDistances :: [(String, String, Int)] -> [((String, String), Int)]
getDistances input = map (\(src, dest, dist) -> ((src, dest), dist)) input ++ map (\(src, dest, dist) -> ((dest, src), dist)) input

locations :: [(String, String, Int)] -> [String]
locations input = nub $ map (\(src, dest, _) -> src) input ++ map (\(_, dest, _) -> dest) input

allRoutes :: [String] -> [[String]]
allRoutes = permutations

routeDistance :: [((String, String), Int)] -> [String] -> Int
routeDistance distances route = sum $ mapMaybe (\(loc1, loc2) -> lookup (loc1, loc2) distances) $ zip route (tail route)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let distances = getDistances $ parseInput input
      locs = locations $ parseInput input
      shortestRoute = minimum $ map (routeDistance distances) (allRoutes locs)
      longestRoute = maximum $ map (routeDistance distances) (allRoutes locs)
  print shortestRoute
  print longestRoute
