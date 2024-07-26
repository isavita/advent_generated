
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    content <- B.readFile "input.txt"
    print $ processStream (B.unpack content) 0 0 False False

processStream :: String -> Int -> Int -> Bool -> Bool -> Int
processStream [] score depth _ _ = score
processStream (x:xs) score depth inGarbage cancelNext
    | cancelNext = processStream xs score depth inGarbage False
    | inGarbage && x == '>' = processStream xs score depth False False
    | inGarbage = processStream xs score depth True (x == '!')
    | otherwise = case x of
        '{' -> processStream xs score (depth + 1) False False
        '}' -> processStream xs (score + depth) (depth - 1) False False
        '<' -> processStream xs score depth True False
        _   -> processStream xs score depth False False
