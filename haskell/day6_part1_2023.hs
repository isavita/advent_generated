
import Data.List
import Data.Char

parseInt :: String -> Int
parseInt = read

calculateWaysToWin :: Int -> Int -> Int
calculateWaysToWin time record = length $ filter (> record) $ map (\holdTime -> holdTime * (time - holdTime)) [1..(time-1)]

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let lines' = lines contents
    let times = map parseInt $ tail $ words $ head lines'
    let distances = map parseInt $ tail $ words $ lines' !! 1
    let totalWays = product $ zipWith calculateWaysToWin times distances
    print totalWays
