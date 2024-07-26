
import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let totalDiff = sum $ map codeDiff (lines contents)
    print totalDiff

codeDiff :: String -> Int
codeDiff s = length s - memoryLength s

memoryLength :: String -> Int
memoryLength s = go (tail (init s)) False 0
  where
    go [] _ len = len
    go ('\\':'x':_:_:xs) _ len = go xs False (len + 1)
    go ('\\':x:xs) _ len = go xs False (len + 1)
    go (x:xs) inEscape len
        | inEscape = go xs False (len + 1)
        | otherwise = go xs False (len + 1)
