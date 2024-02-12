main = do
    contents <- readFile "input.txt"
    let initialState = head $ lines contents
    let resultPart1 = generateChecksum 272 initialState
    let resultPart2 = generateChecksum 35651584 initialState
    putStrLn $ "Part 1: " ++ resultPart1
    putStrLn $ "Part 2: " ++ resultPart2

dragonCurve :: String -> String
dragonCurve a = a ++ "0" ++ (map toggle . reverse $ a)
    where toggle '0' = '1'
          toggle '1' = '0'

fillDisk :: Int -> String -> String
fillDisk size initialState
    | length initialState >= size = take size initialState
    | otherwise = fillDisk size (dragonCurve initialState)

checksum :: String -> String
checksum [] = []
checksum (x:y:xs)
    | x == y = '1' : checksum xs
    | otherwise = '0' : checksum xs

generateChecksum :: Int -> String -> String
generateChecksum size initialState = 
    let filledDisk = fillDisk size initialState
        result = checksum filledDisk
    in if even (length result) then generateChecksum (length result) result else result