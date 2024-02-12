main = do
    input <- readFile "input.txt"
    let rows = 40
        initialRow = head $ lines input
        nextRow row = zipWith3 (\l c r -> if l == '^' && c == '^' && r == '.' || 
                                          c == '^' && r == '^' && l == '.' ||
                                          l == '^' && c == '.' && r == '.' || 
                                          l == '.' && c == '.' && r == '^' then '^' else '.') 
                               ('.':row) row (tail row ++ ".")
        finalMap = take rows $ iterate nextRow initialRow
        safeTiles = sum $ map (length . filter (== '.')) finalMap
    print safeTiles