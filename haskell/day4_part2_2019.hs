
import Data.List (group)
import Data.Maybe (mapMaybe)
import System.IO (readFile)

isValidPassword :: Int -> Bool
isValidPassword password = hasDouble && sorted
  where
    s = show password
    hasDouble = any (\g -> length g == 2) (group s)
    sorted = and $ zipWith (<=) s (tail s)

countValidPasswords :: Int -> Int -> Int
countValidPasswords start end = length $ filter isValidPassword [start..end]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let [start, end] = map read $ wordsWhen (== '-') content
    print $ countValidPasswords start end

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'
