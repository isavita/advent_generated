import Data.List
import Data.Maybe

main = do
    contents <- readFile "input.txt"
    let components = map parseComponent (lines contents)
    let (maxLength, maxStrength) = findLongestBridge components 0 [] 0
    print maxStrength

parseComponent :: String -> (Int, Int)
parseComponent line = let [a, b] = map read (wordsWhen (== '/') line) in (a, b)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

findLongestBridge :: [(Int, Int)] -> Int -> [(Int, Int)] -> Int -> (Int, Int)
findLongestBridge components port used length = maximumBy compareBridges bridges
    where
        bridges = (length, strength) : [findLongestBridge components nextPort ((c1, c2) : used) (length + 1) | (c1, c2) <- components, notElem (c1, c2) used, let nextPort = if c1 == port then c2 else if c2 == port then c1 else -1, nextPort /= -1]
        strength = sum [c1 + c2 | (c1, c2) <- used]
        compareBridges (l1, s1) (l2, s2) = compare l1 l2 <> compare s1 s2