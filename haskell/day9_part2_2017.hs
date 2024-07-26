
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
    content <- B.readFile "input.txt"
    let (garbageCount, _) = processStream (B.unpack content) 0 0 False False 0
    print garbageCount

processStream :: String -> Int -> Int -> Bool -> Bool -> Int -> (Int, Int)
processStream [] score depth _ _ garbageCount = (garbageCount, score)
processStream (x:xs) score depth inGarbage cancelNext garbageCount
    | cancelNext = processStream xs score depth inGarbage False garbageCount
    | inGarbage && x == '!' = processStream xs score depth inGarbage True garbageCount
    | inGarbage && x == '>' = processStream xs score depth False False garbageCount
    | inGarbage = processStream xs score depth True False (garbageCount + 1)
    | x == '{' = processStream xs score (depth + 1) False False garbageCount
    | x == '}' = processStream xs (score + depth) (depth - 1) False False garbageCount
    | x == '<' = processStream xs score depth True False garbageCount
    | otherwise = processStream xs score depth False False garbageCount
