
import qualified Data.Map as Map
import System.IO

-- Function to find the nth number spoken
findNthNumber :: [Int] -> Int -> Int
findNthNumber startingNumbers n = go (Map.fromList initialMap) (length startingNumbers) (last startingNumbers)
  where
    initialMap = zip (init startingNumbers) [1..]
    
    go :: Map.Map Int Int -> Int -> Int -> Int
    go lastSpoken turn current
      | turn == n = current
      | otherwise = 
          let next = case Map.lookup current lastSpoken of
                        Nothing -> 0
                        Just lastTurn -> turn - lastTurn
              newLastSpoken = Map.insert current turn lastSpoken
          in go newLastSpoken (turn + 1) next

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let startingNumbers = map read (words (map (\c -> if c == ',' then ' ' else c) contents)) :: [Int]
    let result = findNthNumber startingNumbers 2020
    print result
