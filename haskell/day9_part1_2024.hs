
import Data.Char (digitToInt, isDigit)
import Data.List (foldl')

main :: IO ()
main = do
  s <- readFile "input.txt"
  let ds = map digitToInt (filter isDigit s)
      disk = expand 0 True ds
      files = [x | Just x <- disk]
      res = take (length files) (compact disk (reverse files))
  print $ foldl' (\a (i, v) -> a + fromIntegral i * fromIntegral v) (0 :: Integer) (zip [0..] res)

expand _ _ [] = []
expand i True (x:xs) = replicate x (Just i) ++ expand (i + 1) False xs
expand i False (x:xs) = replicate x Nothing ++ expand i True xs

compact (Just x:xs) fs = x : compact xs fs
compact (Nothing:xs) (f:fs) = f : compact xs fs
compact _ _ = []
