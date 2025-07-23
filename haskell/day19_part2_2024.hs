
import qualified Data.ByteString.Char8 as B
import Data.List (isPrefixOf)
import Data.Array

main :: IO ()
main = do
    content <- B.readFile "input.txt"
    let ls = B.lines content
        (pats, _:ds) = span (B.any (/= '\n')) ls
        patterns = map B.unpack $ map B.strip $ B.split ',' (B.strip $ head pats)
        designs = map B.unpack ds
    print $ sum $ map (count patterns) designs

count :: [String] -> String -> Integer
count patterns design = dp ! n
  where
    n = length design
    dp = listArray (0, n) [ go i | i <- [0..n] ]
    go 0 = 1
    go i = sum [ dp ! (i - l) | p <- patterns, let l = length p, l <= i, p `isPrefixOf` drop (i - l) design ]
