
import Data.Bits (xor, shiftL)
import Data.Char (isSpace)
import Data.List (intercalate)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let ls = lines content
        a = read . dropWhile isSpace . drop 11 $ head ls
        b = read . dropWhile isSpace . drop 11 $ ls !! 1
        c = read . dropWhile isSpace . drop 11 $ ls !! 2
        prog = map (read . dropWhile isSpace) . split ',' . drop 8 $ ls !! 4
        out = run a b c prog
    putStrLn $ intercalate "," (map show out)

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
    "" -> []
    s' -> w : split c s''
        where (w, s'') = break (== c) s'

run :: Int -> Int -> Int -> [Int] -> [Int]
run a b c prog = go a b c 0
  where
    go a b c ip
        | ip + 1 >= length prog = []
        | otherwise =
            let op = prog !! ip
                operand = prog !! (ip + 1)
                combo o
                    | o <= 3 = o
                    | o == 4 = a
                    | o == 5 = b
                    | o == 6 = c
                    | otherwise = 0
                pow o = 1 `shiftL` combo o
            in case op of
                0 -> go (a `div` pow operand) b c (ip + 2)
                1 -> go a (b `xor` operand) c (ip + 2)
                2 -> go a (combo operand `mod` 8) c (ip + 2)
                3 -> if a /= 0 then go a b c operand else go a b c (ip + 2)
                4 -> go a (b `xor` c) c (ip + 2)
                5 -> combo operand `mod` 8 : go a b c (ip + 2)
                6 -> go a (a `div` pow operand) c (ip + 2)
                7 -> go a b (a `div` pow operand) (ip + 2)
                _ -> go a b c (ip + 1)
