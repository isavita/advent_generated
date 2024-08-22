import Data.Char (ord)
import System.IO (readFile)

hashAlgorithm :: String -> Int
hashAlgorithm input = foldl (\acc c -> (acc + ord c) * 17 `mod` 256) 0 input

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let steps = filter (/= '\n') contents
        sumOfHashes = sum $ map hashAlgorithm $ wordsWhen (== ',') steps
    print sumOfHashes

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s'' where (w, s'') = break p s'