
import Data.List

main = do
    input <- readFile "input.txt"
    let strings = lines input
    let niceStrings1 = filter isNice1 strings
    let niceStrings2 = filter isNice2 strings
    print (length niceStrings1)
    print (length niceStrings2)

isNice1 :: String -> Bool
isNice1 s = hasThreeVowels s && hasDoubleLetter s && not (hasForbiddenSubstring s)

isNice2 :: String -> Bool
isNice2 s = hasPairTwice s && hasRepeatWithOneLetterBetween s

hasThreeVowels :: String -> Bool
hasThreeVowels s = length (filter (`elem` "aeiou") s) >= 3

hasDoubleLetter :: String -> Bool
hasDoubleLetter s = any (\(x, y) -> x == y) (zip s (tail s))

hasForbiddenSubstring :: String -> Bool
hasForbiddenSubstring s = "ab" `isInfixOf` s || "cd" `isInfixOf` s || "pq" `isInfixOf` s || "xy" `isInfixOf` s

hasPairTwice :: String -> Bool
hasPairTwice s = any (\x -> length (filter (==x) (pairs s)) >= 2) (nub (pairs s))

pairs :: [a] -> [[a]]
pairs [] = []
pairs [_] = []
pairs (x:y:zs) = [x,y] : pairs (y:zs)

hasRepeatWithOneLetterBetween :: String -> Bool
hasRepeatWithOneLetterBetween s = any (\(x, _, y) -> x == y) (triples s)

triples :: [a] -> [(a, a, a)]
triples [] = []
triples [_] = []
triples [_,_] = []
triples (x:y:z:zs) = (x, y, z) : triples (y:z:zs)
