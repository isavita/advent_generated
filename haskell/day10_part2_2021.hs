import Data.List
import Data.Maybe
import System.IO

main = do
    contents <- readFile "input.txt"
    let input = lines contents
        scores = mapMaybe (calculateScore . processLine) input
        middleScore = sort scores !! (length scores `div` 2)
    print middleScore

processLine :: String -> Maybe String
processLine line = foldl' processChar (Just []) line
    where processChar Nothing _ = Nothing
          processChar (Just stack) char
              | char `elem` "([{<" = Just (char:stack)
              | otherwise = case stack of
                    (x:xs) | matches x char -> Just xs
                    _ -> Nothing
          matches '(' ')' = True
          matches '[' ']' = True
          matches '{' '}' = True
          matches '<' '>' = True
          matches _ _ = False

calculateScore :: Maybe String -> Maybe Int
calculateScore Nothing = Nothing
calculateScore (Just stack) = Just $ foldl' (\acc char -> acc * 5 + charScore char) 0 stack
    where charScore '(' = 1
          charScore '[' = 2
          charScore '{' = 3
          charScore '<' = 4