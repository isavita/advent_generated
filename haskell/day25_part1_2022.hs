
import Data.Char

fromSnafu :: String -> Int
fromSnafu = foldl (\acc c -> acc * 5 + val c) 0
  where
    val '=' = -2
    val '-' = -1
    val c   = ord c - ord '0'

toSnafu :: Int -> String
toSnafu 0 = ""
toSnafu n = case r of
  3 -> toSnafu q' ++ "="
  4 -> toSnafu q' ++ "-"
  _ -> toSnafu q ++ [chr (ord '0' + r)]
  where
    (q, r) = n `divMod` 5
    q' = q + 1

main :: IO ()
main = do
  input <- readFile "input.txt"
  let snafuLines = lines input
  let sumOfDecimals = sum $ map fromSnafu snafuLines
  putStrLn $ toSnafu sumOfDecimals
