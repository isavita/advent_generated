
import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let pairs = lines contents
        count = length $ filter (\(l, r) -> overlap (parseRange l) (parseRange r)) (map (\x -> (head x, x !! 1)) (map (splitOn ",") pairs))
    print count

splitOn :: String -> String -> [String]
splitOn delim = splitOn' delim []

splitOn' :: String -> String -> String -> [String]
splitOn' _ acc [] = [acc]
splitOn' delim acc (x:xs)
    | x == head delim = acc : splitOn' delim [] xs
    | otherwise = splitOn' delim (acc ++ [x]) xs

parseRange :: String -> (Int, Int)
parseRange s = let [start, end] = map read (splitOn "-" s) in (start, end)

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap (lstart, lend) (rstart, rend) = lstart <= rend && lend >= rstart
