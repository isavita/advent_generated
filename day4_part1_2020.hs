import System.IO
import Data.List

main :: IO ()
main = do
    content <- readFile "input.txt"
    let passports = parsePassports content
        validPassports = filter isValidPassport passports
    print $ length validPassports

parsePassports :: String -> [[String]]
parsePassports = map words . splitOnBlankLines

splitOnBlankLines :: String -> [String]
splitOnBlankLines input = filter (/= "") $ map unlines $ splitWhen null $ lines input

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p s = case dropWhile p s of
                  [] -> []
                  s' -> w : splitWhen p s''
                        where (w, s'') = break p s'

isValidPassport :: [String] -> Bool
isValidPassport passport =
    let requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
        fields = map (takeWhile (/= ':')) passport
    in all (`elem` fields) requiredFields
