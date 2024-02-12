
main = do
    contents <- readFile "input.txt"
    let numbers = map read $ lines contents
    print $ head [x * y | x <- numbers, y <- numbers, x + y == 2020]
