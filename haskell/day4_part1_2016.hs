
import Data.List
import Data.Char

main = do
    contents <- readFile "input.txt"
    let sumOfSectorIDs = sum $ map getSectorID $ filter isRealRoom $ lines contents
    print sumOfSectorIDs

isRealRoom :: String -> Bool
isRealRoom room = let
    parts = splitOn "[" room
    checksum = takeWhile (/= ']') $ last parts
    encryptedName = intercalate "-" $ init $ splitOn "-" $ head parts
    letterCounts = map (\x -> (head x, length x)) $ group $ sort $ filter isAlpha encryptedName
    sortedCounts = sortBy (\x y -> if snd x == snd y then compare (fst x) (fst y) else compare (snd y) (snd x)) letterCounts
    in checksum == map fst (take 5 sortedCounts)

getSectorID :: String -> Int
getSectorID room = read $ takeWhile isDigit $ last $ splitOn "-" room

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = []
splitOn delim str = first : splitOn delim rest
    where (first, rest) = breakList delim str

breakList :: Eq a => [a] -> [a] -> ([a], [a])
breakList _ [] = ([], [])
breakList sublist list@(x:xs)
    | sublist `isPrefixOf` list = ([], drop (length sublist) list)
    | otherwise = let (found, rest) = breakList sublist xs in (x : found, rest)
