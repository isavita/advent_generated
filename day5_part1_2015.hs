
import Data.List

isNice :: String -> Bool
isNice s = hasThreeVowels s && hasDoubleLetter s && not (hasForbiddenSubstrings s)

hasThreeVowels :: String -> Bool
hasThreeVowels = (>= 3) . length . filter (`elem` "aeiou")

hasDoubleLetter :: String -> Bool
hasDoubleLetter (x:y:xs)
  | x == y = True
  | otherwise = hasDoubleLetter (y:xs)
hasDoubleLetter _ = False

hasForbiddenSubstrings :: String -> Bool
hasForbiddenSubstrings s = any (`isInfixOf` s) ["ab", "cd", "pq", "xy"]

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    print $ length $ filter isNice input
