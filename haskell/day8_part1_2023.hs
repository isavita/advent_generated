
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Char (isUpper)

parseLine :: String -> (String, (String, String))
parseLine line = (key, (left, right))
  where
    key = take 3 line
    left = take 3 $ drop 7 line
    right = take 3 $ drop 12 line

solve :: String -> M.Map String (String, String) -> Int
solve instructions desertMap = go "AAA" instructions 0
  where
    go current (i:is) steps
      | current == "ZZZ" = steps
      | otherwise = go next (is ++ [i]) (steps + 1)
      where
        next = case i of
          'L' -> fst $ fromJust $ M.lookup current desertMap
          'R' -> snd $ fromJust $ M.lookup current desertMap
    go _ [] _ = error "Should not happen: Empty instruction list"

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let instructions = head ls
  let desertMap = M.fromList $ map parseLine $ drop 2 ls
  print $ solve instructions desertMap
