
import Data.List
import System.IO

sortString :: String -> String
sortString = sort

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let passphrases = lines contents
        validCount = length [p | p <- passphrases, isValidPassphrase p]
    print validCount

isValidPassphrase :: String -> Bool
isValidPassphrase passphrase = 
    let wordsList = words passphrase
        sortedWords = map sortString wordsList
    in length (nub sortedWords) == length sortedWords
