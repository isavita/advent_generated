
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import System.IO (readFile)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let tlsCount = length . filter supportsTLS $ lines content
    print tlsCount

supportsTLS :: String -> Bool
supportsTLS ip = not (any containsABBA bracketContents) && containsABBA (filter (/= '-') strippedIp)
  where
    bracketContents = filter (not . null) $ splitOn "[" ip >>= splitOn "]"
    strippedIp = concat $ splitOn "[" ip

containsABBA :: String -> Bool
containsABBA s = any isABBA (zip3 s (tail s) (tail (tail s)))
  where
    isABBA (a, b, c) = a /= b && a == c && b == (head (tail (tail (tail s))))
