import System.IO
import Data.List

main :: IO ()
main = do
    content <- readFile "input.txt"
    let groups = splitGroups content
        counts = map (length . nub . filter (/= '\n')) groups
    print $ sum counts

splitGroups :: String -> [String]
splitGroups input = let groupedLines = groupBy (\a b -> not (null a) && not (null b)) (lines input)
                    in map unlines groupedLines

