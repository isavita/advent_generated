
import Data.Char
import Data.List

readInput :: FilePath -> IO String
readInput filename = readFile filename

incrementPassword :: String -> String
incrementPassword = reverse . go . reverse
  where
    go [] = []
    go (x:xs)
      | x == 'z' = 'a' : go xs
      | otherwise = succ x : xs

hasStraight :: String -> Bool
hasStraight password = any (\(x,y,z) -> ord y == ord x + 1 && ord z == ord y + 1) $ zip3 password (tail password) (tail (tail password))

containsInvalidLetters :: String -> Bool
containsInvalidLetters password = 'i' `elem` password || 'o' `elem` password || 'l' `elem` password

hasTwoPairs :: String -> Bool
hasTwoPairs password = (>= 2) . length . filter (== True) . map (\x -> length x >= 2) . group $ password

isValidPassword :: String -> Bool
isValidPassword password = hasStraight password && not (containsInvalidLetters password) && hasTwoPairs password

findNextPassword :: String -> String
findNextPassword password = head $ filter isValidPassword $ iterate incrementPassword password

main :: IO ()
main = do
    currentPassword <- fmap (filter (/= '\n')) $ readInput "input.txt"
    let firstNewPassword = findNextPassword (incrementPassword currentPassword)
    let secondNewPassword = findNextPassword (incrementPassword firstNewPassword)
    putStrLn secondNewPassword
