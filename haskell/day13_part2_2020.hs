import System.IO
import Data.List
import Data.Maybe

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let busIDs = map parseBusID . zip [0..] . splitOn ',' $ (lines contents !! 1)
    let buses = catMaybes busIDs
    let timestamp = findTimestamp buses
    print timestamp

findTimestamp :: [(Int, Int)] -> Int
findTimestamp buses = foldl' findTime 0 buses
  where
    findTime timestamp (offset, busID) = head [t | t <- [timestamp, timestamp+step..], (t + offset) `mod` busID == 0]
      where step = product [bus | (_, bus) <- takeWhile (/= (offset, busID)) buses]

parseBusID :: (Int, String) -> Maybe (Int, Int)
parseBusID (index, "x") = Nothing
parseBusID (index, busID) = Just (index, read busID)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delimiter = foldr f [[]]
  where
    f c l@(x:xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs