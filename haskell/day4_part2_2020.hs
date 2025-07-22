
import Data.List (isInfixOf)
import Data.Char (isDigit, isHexDigit)
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    content <- TIO.readFile "input.txt"
    let passports = map T.unpack $ T.splitOn (T.pack "\n\n") content
    print $ length $ filter isValidPassport passports

isValidPassport :: String -> Bool
isValidPassport s = all (`isInfixOf` s) ["byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:"] &&
    all checkField ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
  where
    checkField f = case lookupField f s of
        Just v -> case f of
            "byr" -> validateYear v 1920 2002
            "iyr" -> validateYear v 2010 2020
            "eyr" -> validateYear v 2020 2030
            "hgt" -> validateHgt v
            "hcl" -> validateHcl v
            "ecl" -> validateEcl v
            "pid" -> validatePid v
            _ -> False
        Nothing -> False

lookupField :: String -> String -> Maybe String
lookupField field s = do
    let prefix = field ++ ":"
    case dropWhile (not . isPrefixOf prefix) (words s) of
        [] -> Nothing
        (x:_) -> Just $ drop (length prefix) x
  where
    isPrefixOf pre str = take (length pre) str == pre

validateYear :: String -> Int -> Int -> Bool
validateYear s min max = case readMaybe s of
    Just y -> y >= min && y <= max
    Nothing -> False

validateHgt :: String -> Bool
validateHgt s
    | "cm" `isSuffixOf` s = case readMaybe (takeWhile isDigit s) of
        Just h -> h >= 150 && h <= 193
        Nothing -> False
    | "in" `isSuffixOf` s = case readMaybe (takeWhile isDigit s) of
        Just h -> h >= 59 && h <= 76
        Nothing -> False
    | otherwise = False
  where
    isSuffixOf suf str = drop (length str - length suf) str == suf

validateHcl :: String -> Bool
validateHcl s = length s == 7 && head s == '#' && all isHexDigit (tail s)

validateEcl :: String -> Bool
validateEcl s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validatePid :: String -> Bool
validatePid s = length s == 9 && all isDigit s
