
main = do
    input <- readFile "input.txt"
    let input' = head . lines $ input
        halfway = length input' `div` 2
        result = sum [read [x] | (i, x) <- zip [0..] input', let next = (i + halfway) `mod` length input', x == input' !! next]
    print result
