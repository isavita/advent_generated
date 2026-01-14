
import System.IO (readFile)
import Data.List (sortOn, foldl')
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

parseRange :: String -> Maybe (Int, Int)
parseRange s = case break (== '-') s of
    (a, '-':b) -> do
        x <- readMaybe (trim a)
        y <- readMaybe (trim b)
        let (lo, hi) = if x <= y then (x, y) else (y, x)
        return (lo, hi)
    _ -> Nothing
  where
    trim = f . f
      where f = reverse . dropWhile (== ' ')

main :: IO ()
main = do
    content <- readFile "input.txt"
    let ranges = mapMaybe parseRange . filter (not . null) $ lines content
    if null ranges
        then putStrLn "Total fresh IDs: 0"
        else
            let sorted = sortOn fst ranges
                (cMin, cMax, acc) = foldl' step (fst (head sorted), snd (head sorted), 0) (tail sorted)
                total = acc + cMax - cMin + 1
            in putStrLn $ "Total fresh IDs: " ++ show total
  where
    step (curL, curR, tot) (l, r)
        | l <= curR = (curL, max curR r, tot)
        | otherwise = (l, r, tot + curR - curL + 1)
