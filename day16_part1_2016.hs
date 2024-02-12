
main = do
    input <- readFile "input.txt"
    let initial = head $ lines input
    let diskLength = 272
    let dataToFill = generateData initial diskLength
    let checksum = calculateChecksum dataToFill
    putStrLn checksum

dragonCurve :: String -> String
dragonCurve a = a ++ "0" ++ (map (\x -> if x == '0' then '1' else '0') (reverse a))

generateData :: String -> Int -> String
generateData a n
    | length a >= n = take n a
    | otherwise = generateData (dragonCurve a) n

calculateChecksum :: String -> String
calculateChecksum s
    | even (length s) = calculateChecksum (reduceChecksum s)
    | otherwise = s

reduceChecksum :: String -> String
reduceChecksum [] = []
reduceChecksum (x:y:xs)
    | x == y = '1' : reduceChecksum xs
    | otherwise = '0' : reduceChecksum xs
