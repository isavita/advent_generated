
main :: IO ()
main = do
    contents <- readFile "input.txt"
    let commands = lines contents
        (horizontalPosition, depth) = foldl processCommand (0, 0) commands
        product = horizontalPosition * depth
    print product

processCommand :: (Int, Int) -> String -> (Int, Int)
processCommand (horizontalPosition, depth) command = case words command of
    ["forward", unitsStr] -> (horizontalPosition + read unitsStr, depth)
    ["down", unitsStr] -> (horizontalPosition, depth + read unitsStr)
    ["up", unitsStr] -> (horizontalPosition, depth - read unitsStr)
    _ -> (horizontalPosition, depth)
