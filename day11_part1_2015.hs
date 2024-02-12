
import Data.List
import Data.Char (ord)

main :: IO ()
main = do
    currentPassword <- readFile "input.txt"
    let newPassword = findNextPassword currentPassword
    putStrLn newPassword

findNextPassword :: String -> String
findNextPassword password = head $ filter isValidPassword $ iterate incrementPassword password

incrementPassword :: String -> String
incrementPassword = reverse . carryOver . reverse
    where
        carryOver [] = "a"
        carryOver (x:xs)
            | x == 'z' = 'a' : carryOver xs
            | otherwise = succ x : xs

isValidPassword :: String -> Bool
isValidPassword password = hasStraight password && not (containsInvalidLetters password) && hasTwoPairs password

hasStraight :: String -> Bool
hasStraight (a:b:c:xs) = (ord a + 1 == ord b) && (ord b + 1 == ord c) || hasStraight (b:c:xs)
hasStraight _ = False

containsInvalidLetters :: String -> Bool
containsInvalidLetters = any (`elem` "iol")

hasTwoPairs :: String -> Bool
hasTwoPairs = (>= 2) . length . filter ((>= 2) . length) . group
