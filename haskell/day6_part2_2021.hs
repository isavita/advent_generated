
import qualified Data.Map.Strict as M
import Data.List (foldl')
import Data.Char (isDigit)

type Counter = M.Map Int Integer

main :: IO ()
main = do
    content <- readFile "input.txt"
    let nums = parse content
        counter0 = foldl' (\m k -> M.insertWith (+) k 1 m) (M.fromList [(i,0) | i <- [0..8]]) nums
        part2 = simulate counter0 256
        part1 = simulate counter0 80
    print part1
    print part2

simulate :: Counter -> Int -> Integer
simulate counter days = go counter days
  where
    go c 0 = sum c
    go c n =
        let z = M.findWithDefault 0 0 c
            c' = M.fromList
                [ (0, M.findWithDefault 0 1 c)
                , (1, M.findWithDefault 0 2 c)
                , (2, M.findWithDefault 0 3 c)
                , (3, M.findWithDefault 0 4 c)
                , (4, M.findWithDefault 0 5 c)
                , (5, M.findWithDefault 0 6 c)
                , (6, M.findWithDefault 0 7 c + z)
                , (7, M.findWithDefault 0 8 c)
                , (8, z)
                ]
        in go c' (n - 1)

parse :: String -> [Int]
parse = map read . words . map (\c -> if isDigit c then c else ' ')
