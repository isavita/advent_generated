import System.IO
import Data.List

main = do
    contents <- readFile "input.txt"
    let initialFish = map read $ wordsWhen (==',') contents
    let fishCounts = foldl' (\acc x -> updateFishCounts acc x) (replicate 9 0) initialFish
    let finalCounts = simulateDays 80 fishCounts
    print $ sum finalCounts

updateFishCounts :: [Int] -> Int -> [Int]
updateFishCounts acc x = take x acc ++ [acc !! x + 1] ++ drop (x + 1) acc

simulateDays :: Int -> [Int] -> [Int]
simulateDays 0 fishCounts = fishCounts
simulateDays days fishCounts = simulateDays (days - 1) (simulateDay fishCounts)

simulateDay :: [Int] -> [Int]
simulateDay fishCounts = let
    newFish = head fishCounts
    updatedCounts = tail fishCounts ++ [newFish]
    in take 6 updatedCounts ++ [updatedCounts !! 6 + newFish] ++ drop 7 updatedCounts

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s'' where (w, s'') = break p s'