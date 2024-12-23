
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map as Map

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let rooms = lines contents
  print $ fromJust $ find (not . null) $ map findNorthPole rooms

findNorthPole :: String -> String
findNorthPole room =
  if isRealRoom room
    then
      let decrypted = decryptName room
      in if "northpole object" `isInfixOf` decrypted
           then show $ getSectorID room
           else ""
    else ""

isRealRoom :: String -> Bool
isRealRoom room =
  let (namePart, checksumPart) = splitAt (length room - 7) room
      checksum = init $ tail checksumPart
      nameParts = init $ splitOn '-' namePart
      letterCounts = foldl (\m s -> foldl (\m' c -> Map.insertWith (+) c 1 m') m s) Map.empty nameParts
      sortedCounts = sortBy (\(c1, n1) (c2, n2) -> if n1 == n2 then compare c1 c2 else compare n2 n1) $ Map.toList letterCounts
      top5 = map fst $ take 5 sortedCounts
  in top5 == checksum

getSectorID :: String -> Int
getSectorID room =
  let parts = splitOn '-' room
      sectorIDPart = last parts
  in read $ takeWhile isDigit sectorIDPart

decryptName :: String -> String
decryptName room =
  let parts = splitOn '-' room
      sectorID = getSectorID room
      nameParts = init parts
  in unwords $ map (map (decryptChar sectorID)) nameParts

decryptChar :: Int -> Char -> Char
decryptChar sectorID c
  | isLower c = chr $ ord 'a' + ( (ord c - ord 'a' + sectorID) `mod` 26)
  | otherwise = ' '

splitOn :: Char -> String -> [String]
splitOn delimiter str = case dropWhile (== delimiter) str of
    "" -> []
    s -> w : splitOn delimiter s'
         where (w, s') = break (== delimiter) s
