
main :: IO ()
main = do
    contents <- readFile "input.txt"
    let expenses = map read $ lines contents
    let result = head [x * y * z | x <- expenses, y <- expenses, z <- expenses, x + y + z == 2020]
    print result
