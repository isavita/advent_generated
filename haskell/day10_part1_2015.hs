
import System.IO

main = do
    contents <- readFile "input.txt"
    let result = lookAndSay contents 40
    print $ length result

lookAndSay :: String -> Int -> String
lookAndSay sequence iterations = iterate nextSequence sequence !! iterations

nextSequence :: String -> String
nextSequence sequence = concatMap (\g -> show (length g) ++ [head g]) $ group sequence

group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = (x : takeWhile (== x) xs) : group (dropWhile (== x) xs)
