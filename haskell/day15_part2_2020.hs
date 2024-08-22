import qualified Data.Map as Map
import System.IO

main = do
    contents <- readFile "input.txt"
    let startingNumbers = map read $ words $ map (\c -> if c == ',' then ' ' else c) contents
    let result = playGame startingNumbers 30000000
    print result

playGame :: [Int] -> Int -> Int
playGame startingNumbers turns = go (length startingNumbers) (last startingNumbers) initialMap
  where
    initialMap = Map.fromList $ zip (init startingNumbers) [1..]
    go turn lastNum history
        | turn == turns = lastNum
        | otherwise = let nextNum = case Map.lookup lastNum history of
                                        Nothing -> 0
                                        Just prevTurn -> turn - prevTurn
                      in nextNum `seq` go (turn + 1) nextNum (Map.insert lastNum turn history)