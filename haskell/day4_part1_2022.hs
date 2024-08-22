import System.IO

main = do
    contents <- readFile "input.txt"
    let count = length [() | line <- lines contents, let [range1, range2] = map parseRange (wordsWhen (==',') line), contains range1 range2 || contains range2 range1]
    print count

data Range = Range Int Int

parseRange :: String -> Range
parseRange s = let [start, end] = map read (wordsWhen (=='-') s) in Range start end

contains :: Range -> Range -> Bool
contains (Range start1 end1) (Range start2 end2) = start1 <= start2 && end1 >= end2

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s'' where (w, s'') = break p s'