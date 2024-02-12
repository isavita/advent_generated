
import Data.List
import Data.Char

main = do
    contents <- readFile "input.txt"
    let positions = sort $ map read $ concatMap (wordsWhen (== ',')) $ lines contents
    let minFuel = minimum [sum [calculateNewFuel pos i | pos <- positions] | i <- [head positions..last positions]]
    print minFuel

calculateNewFuel :: Int -> Int -> Int
calculateNewFuel currentPosition newPosition = sum [(customAbs (currentPosition - newPosition) * (customAbs (currentPosition - newPosition) + 1)) `div` 2]

customAbs :: Int -> Int
customAbs n = if n < 0 then -n else n

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                          where (w, s'') = break p s'
