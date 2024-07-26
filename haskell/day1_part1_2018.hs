
import System.IO

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let freqChanges = lines contents
        freq = sum $ map parseChange freqChanges
    print freq

parseChange :: String -> Int
parseChange (x:xs) = sign * read xs
  where sign = if x == '-' then -1 else 1
