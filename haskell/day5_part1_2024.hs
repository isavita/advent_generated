
import Data.List
import qualified Data.Map as M

parseRule :: String -> (Int, Int)
parseRule s = let [x, y] = map read $ wordsWhen (=='|') s in (x, y)

parseUpdate :: String -> [Int]
parseUpdate = map read . wordsWhen (==',')

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where (w, s'') = break p s'

isCorrect :: [(Int, Int)] -> [Int] -> Bool
isCorrect rules update =
  let pos = M.fromList $ zip update [0..]
  in all (\(x, y) -> case (M.lookup x pos, M.lookup y pos) of
        (Just px, Just py) -> px < py
        _ -> True) rules

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ls = filter (not . null) $ lines contents
  let (ruleLines, updateLines) = span (elem '|') ls
  let rules = map parseRule ruleLines
  let updates = map parseUpdate updateLines
  let correctUpdates = filter (isCorrect rules) updates
  let result = sum [u !! (length u `div` 2) | u <- correctUpdates]
  print result
