
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ length $ filter supportsSSL $ lines input

supportsSSL :: String -> Bool
supportsSSL ip = 
    let bracketContents = extractBracketContents ip
        outsideABAs = findABAs $ removeBrackets ip
    in any (\aba -> let bab = [aba!!1, aba!!0, aba!!1] 
                    in any (bab `isInfixOf`) bracketContents) outsideABAs

extractBracketContents :: String -> [String]
extractBracketContents = go []
  where
    go acc [] = reverse acc
    go acc (x:xs)
      | x == '[' = let (bracket, rest) = break (==']') xs
                   in go (bracket : acc) (drop 1 rest)
      | otherwise = go acc xs

removeBrackets :: String -> String
removeBrackets = go []
  where
    go acc [] = reverse acc
    go acc (x:xs)
      | x == '[' = go acc (dropWhile (/=']') xs)
      | x == ']' = go acc xs
      | otherwise = go (x:acc) xs

findABAs :: String -> [[Char]]
findABAs s = [take 3 $ drop i s | 
              i <- [0 .. length s - 3], 
              let sub = take 3 $ drop i s,
              length sub == 3 && 
              sub!!0 /= sub!!1 && 
              sub!!0 == sub!!2]
