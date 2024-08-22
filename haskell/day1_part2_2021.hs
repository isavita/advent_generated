import System.IO

main = do
    contents <- readFile "input.txt"
    let measurements = map read (lines contents) :: [Int]
        slidingSums = zipWith3 (\a b c -> a + b + c) measurements (drop 1 measurements) (drop 2 measurements)
        count = length $ filter (\(a, b) -> b > a) $ zip slidingSums (drop 1 slidingSums)
    print count