
main :: IO ()
main = do
    contents <- readFile "input.txt"
    let totalDiff = sum $ map (\line -> calculateEncodedLength line - length line) (lines contents)
    print totalDiff

calculateEncodedLength :: String -> Int
calculateEncodedLength s = 2 + length (filter (`elem` ['\\', '\"']) s) + length s
