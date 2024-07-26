
import Data.List (sort)
import System.IO (readFile)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let totalRibbon = sum $ map calculateRibbon (lines contents)
    print totalRibbon

calculateRibbon :: String -> Int
calculateRibbon dims = bow + wrap
  where
    [l, w, h] = map read (wordsWhen (== 'x') dims)
    bow = l * w * h
    wrap = 2 * (smallest + secondSmallest)
    [smallest, secondSmallest] = take 2 . sort $ [l, w, h]

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'
