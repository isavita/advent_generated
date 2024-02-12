
import Data.Char
import Data.List

react :: String -> String
react polymer = foldr react' [] polymer
    where
        react' :: Char -> String -> String
        react' x (y:ys)
            | reacts x y = ys
            | otherwise = x : y : ys
        react' x [] = [x]

        reacts :: Char -> Char -> Bool
        reacts x y = toUpper x == toUpper y && x /= y

main :: IO ()
main = do
    content <- readFile "input.txt"
    let polymer = head (lines content)

    let allUnits = ['a'..'z']
    let minLength = minimum [length (react (filter (\c -> toLower c /= unit) polymer)) | unit <- allUnits]

    print minLength
