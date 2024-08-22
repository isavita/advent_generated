import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let banks = map read $ words contents
    let seenConfigs = Map.empty
    let (cycleCount, loopSize) = reallocate banks seenConfigs 0
    print loopSize

reallocate :: [Int] -> Map.Map [Int] Int -> Int -> (Int, Int)
reallocate banks seenConfigs cycleCount
    | banks `Map.member` seenConfigs = (cycleCount, cycleCount - seenConfigs Map.! banks)
    | otherwise = reallocate newBanks newSeenConfigs (cycleCount + 1)
  where
    newSeenConfigs = Map.insert banks cycleCount seenConfigs
    (maxBlocks, maxIndex) = maximumWithIndex banks
    newBanks = redistribute banks maxBlocks maxIndex

maximumWithIndex :: [Int] -> (Int, Int)
maximumWithIndex xs = foldl (\(maxVal, maxIdx) (val, idx) -> if val > maxVal then (val, idx) else (maxVal, maxIdx)) (head xs, 0) (zip xs [0..])

redistribute :: [Int] -> Int -> Int -> [Int]
redistribute banks blocks index = foldl (\acc i -> let newIndex = (index + i + 1) `mod` length banks in adjustList (+1) newIndex acc) (adjustList (const 0) index banks) [0..blocks-1]

adjustList :: (Int -> Int) -> Int -> [Int] -> [Int]
adjustList f idx xs = take idx xs ++ [f (xs !! idx)] ++ drop (idx + 1) xs