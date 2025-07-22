
import Data.Char (isDigit)
import System.IO (readFile)

main :: IO ()
main = do
    s <- readFile "input.txt"
    print $ go True s 0
  where
    go _ [] acc = acc
    go enabled s acc
        | "mul(" `isPrefixOf` s, enabled =
            let (a, rest) = parseNum (drop 4 s)
                (b, rest') = if null rest || head rest /= ',' then (0, []) else parseNum (tail rest)
            in if null rest' || head rest' /= ')' then go enabled (tail s) acc
               else go enabled (tail rest') (acc + a * b)
        | "do()" `isPrefixOf` s = go True (drop 4 s) acc
        | "don't()" `isPrefixOf` s = go False (drop 7 s) acc
        | otherwise = go enabled (tail s) acc

    isPrefixOf pre str = take (length pre) str == pre

    parseNum s = case span isDigit s of
        (ds, rest) -> (read ds, rest)
