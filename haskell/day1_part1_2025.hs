
import Data.Char (isDigit)

main :: IO ()
main = do
  txt <- readFile "input.txt"
  print . snd $ foldl go (50, 0) (words txt)
  where
    go (pos, cnt) ('R':xs) = let p = (pos + read xs) `mod` 100 in (p, if p == 0 then cnt + 1 else cnt)
    go (pos, cnt) ('L':xs) = let p = (pos - read xs) `mod` 100 in (p, if p == 0 then cnt + 1 else cnt)
    go s _ = s
