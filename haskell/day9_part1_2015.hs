import System.IO
import Data.List
import Data.Maybe
import qualified Data.Map as Map

main = do
    contents <- readFile "input.txt"
    let distances = map parseLine (lines contents)
        locations = nub $ concatMap (\(a, b, _) -> [a, b]) distances
        distMap = foldl (\m (a, b, d) -> Map.insert (a, b) d $ Map.insert (b, a) d m) Map.empty distances
        shortestRoute = minimum $ map (calculateDistance distMap) (permutations locations)
    print shortestRoute

parseLine :: String -> (String, String, Int)
parseLine line = let [from, _, to, _, dist] = words line in (from, to, read dist)

calculateDistance :: Map.Map (String, String) Int -> [String] -> Int
calculateDistance distMap route = sum $ mapMaybe (\(a, b) -> Map.lookup (a, b) distMap) (zip route (tail route))