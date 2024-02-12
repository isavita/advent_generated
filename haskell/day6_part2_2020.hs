import System.IO
import Data.List

main :: IO ()
main = do
    content <- readFile "input.txt"
    let groups = map lines . splitGroups $ content
        counts = map (length . questionsEveryoneAnsweredYes) groups
    print $ sum counts

splitGroups :: String -> [String]
splitGroups input = let groupedLines = groupBy (\a b -> not (null a) && not (null b)) (lines input)
                    in map unlines groupedLines

questionsEveryoneAnsweredYes :: [String] -> String
questionsEveryoneAnsweredYes = foldr1 intersect . map nub
