
import Data.List
import System.IO

main = do
    contents <- readFile "input.txt"
    let passphrases = lines contents
        validCount = length $ filter isValidPassphrase passphrases
    print validCount

isValidPassphrase :: String -> Bool
isValidPassphrase passphrase = let wordsList = words passphrase
                                   wordSet = foldl (\acc word -> if word `elem` acc then acc else word:acc) [] wordsList
                               in length wordsList == length wordSet
