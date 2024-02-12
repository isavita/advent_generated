
import Data.List

main = do
    contents <- readFile "input.txt"
    let initialSequence = head (lines contents)
    let result = lookAndSay initialSequence 50
    print (length result)

lookAndSay :: String -> Int -> String
lookAndSay sequence iterations = foldl' (\acc _ -> nextSequence acc) sequence [1..iterations]

nextSequence :: String -> String
nextSequence sequence = concatMap (\g -> show (length g) ++ [head g]) (group sequence)
