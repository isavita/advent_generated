
import qualified Data.ByteString.Char8 as B
import Data.List (isPrefixOf)
import Data.Array

main :: IO ()
main = do
    content <- B.readFile "input.txt"
    let ls = B.lines content
        (availLine : _ : rest) = ls
        patterns = map B.unpack $ map B.strip $ B.split ',' availLine
        designs = map B.unpack rest
        ans = length $ filter (canMake patterns) designs
    print ans

canMake :: [String] -> String -> Bool
canMake ps design = dp ! n
  where
    n = length design
    dp = listArray (0, n) [ go i | i <- [0..n] ]
    go 0 = True
    go i = any (\p -> let l = length p
                      in l <= i && dp ! (i - l) && p `isPrefixOf` drop (i - l) design) ps
