import System.IO
import Data.Char (isSpace, isDigit)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let decompressedLength = calculateDecompressedLength (filter (not . isSpace) contents)
    print decompressedLength

calculateDecompressedLength :: String -> Int
calculateDecompressedLength input = go input 0
  where
    go [] len = len
    go (x:xs) len
        | x == '(' = let (marker, rest) = span (/= ')') xs
                         (numChars, repeatCount) = parseMarker marker
                         (toRepeat, remaining) = splitAt numChars (tail rest)
                     in go remaining (len + length toRepeat * repeatCount)
        | otherwise = go xs (len + 1)

parseMarker :: String -> (Int, Int)
parseMarker marker = let (numChars, repeatCount) = break (== 'x') marker
                     in (read numChars, read (tail repeatCount))