import System.IO
import Data.List

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let blockedRanges = map parseRange (lines contents)
        sortedRanges = sortOn fst blockedRanges
        lowestAllowedIP = findLowestAllowedIP sortedRanges
    print lowestAllowedIP

parseRange :: String -> (Int, Int)
parseRange s = let [start, end] = map read (wordsWhen (== '-') s) in (start, end)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

findLowestAllowedIP :: [(Int, Int)] -> Int
findLowestAllowedIP ranges = go ranges 0
    where
        go [] currentIP = currentIP
        go ((start, end):rs) currentIP
            | currentIP < start = currentIP
            | otherwise = go rs (max currentIP (end + 1))