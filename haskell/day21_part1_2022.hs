
import Data.Map (Map, fromList, (!), insert, member)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.IO

calculate :: String -> Map String String -> Map String Integer -> (Integer, Map String Integer)
calculate monkey jobs results =
  case member monkey results of
    True -> (results ! monkey, results)
    False ->
      case member monkey jobs of
        False -> error $ "Monkey not found: " ++ monkey
        True ->
          let job = jobs ! monkey
          in case readMaybe job :: Maybe Integer of
               Just num -> (num, insert monkey num results)
               Nothing ->
                 let parts = words job
                     (a, results') = calculate (parts !! 0) jobs results
                     (b, results'') = calculate (parts !! 2) jobs results'
                     result = case parts !! 1 of
                       "+" -> a + b
                       "-" -> a - b
                       "*" -> a * b
                       "/" -> div a b
                       _ -> error $ "Unknown operation: " ++ (parts !! 1)
                 in (result, insert monkey result results'')

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let linesList = lines contents
      jobsList = map (\line -> let parts = splitOn ": " line in (parts !! 0, parts !! 1)) linesList
      jobs = fromList jobsList
      (result, _) = calculate "root" jobs mempty
  print result
  hClose handle

splitOn :: String -> String -> [String]
splitOn delimiter string = splitOn' delimiter string []
  where
    splitOn' :: String -> String -> [String] -> [String]
    splitOn' delimiter string acc =
      case break (== head delimiter) string of
        (before, "") -> reverse (before : acc)
        (before, after) ->
          let remaining = drop (length delimiter) after
          in splitOn' delimiter remaining (before : acc)
