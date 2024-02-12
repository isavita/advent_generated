
main = do
    contents <- readFile "input.txt"
    let inputLines = map words $ lines contents
        checksum = sum [maximum nums - minimum nums | nums <- map (map read) inputLines]
    print checksum
