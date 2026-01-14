
import System.IO (readFile)
import Data.List (foldl')
import Control.DeepSeq (force)

dialSize :: Int
dialSize = 100

process :: (Int, Int) -> String -> (Int, Int)
process (cur, tot) line = case line of
    []       -> (cur, tot)
    (d:rs)   ->
        let amt = read rs :: Int in
        case d of
            'R' ->
                let newCur = (cur + amt) `mod` dialSize
                    add    = (cur + amt) `div` dialSize
                in (newCur, tot + add)
            'L' ->
                let newCur = (cur - amt) `mod` dialSize
                    add    = (cur - 1) `div` dialSize - (cur - amt - 1) `div` dialSize
                in (newCur, tot + add)
            _   -> error $ "Unknown direction " ++ [d]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let linesNonEmpty = filter (not . null) . map (takeWhile (/= '\r')) $ lines content
        (_, total) = force $ foldl' process (50, 0) linesNonEmpty
    putStrLn $ "The password is: " ++ show total
