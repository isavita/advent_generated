
import Data.Word (Word64)
import Data.List (foldl')
import Data.Char (isDigit)

isInvalid :: Word64 -> Bool
isInvalid x =
  let s = show x
      n = length s
      ok p = drop p s == take (n - p) s
  in n > 1 && any (\p -> n `mod` p == 0 && ok p) [1 .. n `div` 2]

parse :: String -> [(Word64, Word64)]
parse s = case dropWhile (not . isDigit) s of
  "" -> []
  s1 -> case span isDigit s1 of
    (aStr, '-':s2) -> case span isDigit s2 of
      ("", _) -> []
      (bStr, s3) ->
        let a = read aStr :: Word64
            b = read bStr :: Word64
        in (min a b, max a b) : parse s3
    _ -> []

main :: IO ()
main = do
  txt <- readFile "input.txt"
  let pairs = parse txt
      ans = foldl' (\acc (lo, hi) -> foldl' (\a x -> if isInvalid x then a + x else a) acc [lo..hi]) 0 pairs
  print (ans :: Word64)
