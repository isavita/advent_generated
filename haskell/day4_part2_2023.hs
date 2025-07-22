
import Data.List (intersect)
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    txt <- TIO.readFile "input.txt"
    let ls = filter (not . T.null) $ T.lines txt
        cards = map parseCard ls
        counts = foldl (\acc (i, pts) -> let n = acc !! i
                                         in zipWith (\j c -> if j > i && j <= i + pts then c + n else c) [0..] acc)
                       (replicate (length cards) 1)
                       (zip [0..] (map score cards))
    print $ sum counts

parseCard :: T.Text -> [Int]
parseCard t = let [_, rest] = T.splitOn (T.pack ": ") t
                  [win, have] = T.splitOn (T.pack " | ") rest
                  ws = map (read . T.unpack) $ filter (not . T.null) $ T.split (== ' ') win
                  hs = map (read . T.unpack) $ filter (not . T.null) $ T.split (== ' ') have
              in ws `intersect` hs

score :: [Int] -> Int
score = length
