
import Data.List
import Data.Function

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let messages = lines contents
        originalMessage = getOriginalMessage messages
    putStrLn originalMessage

getOriginalMessage :: [String] -> String
getOriginalMessage [] = ""
getOriginalMessage messages = map (getLeastCommonChar . map head) $ transpose (map toChars messages)
    where toChars = map (\c -> [c])

getLeastCommonChar :: String -> Char
getLeastCommonChar chars = head $ minimumBy (compare `on` length) $ group $ sort chars
