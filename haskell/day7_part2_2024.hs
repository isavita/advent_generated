
import Data.Char
import Data.List
import Data.Maybe

concatInt :: Int -> Int -> Int
concatInt a b = read $ show a ++ show b

canProduce :: Int -> [Int] -> Bool
canProduce target nums = any (== target) $ go 0 0
  where
    go idx val
      | idx == length nums = [val]
      | otherwise =
          let n = nums !! idx
          in  go (idx + 1) (val + n) ++
              go (idx + 1) (val * n) ++
              if val == 0 then [] else go (idx + 1) (concatInt val n)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let total = sum $ mapMaybe processLine $ lines contents
  print total

processLine :: String -> Maybe Int
processLine line
  | null line = Nothing
  | otherwise =
    let parts = splitOn ":" line
    in case parts of
      [targetStr, numsStr] ->
        let target = read $ trim targetStr :: Int
            nums = map (read :: String -> Int) $ words $ trim numsStr
        in if null nums
           then Nothing
           else if length nums == 1
                then if head nums == target then Just target else Nothing
                else if canProduce target nums then Just target else Nothing
      _ -> Nothing

splitOn :: String -> String -> [String]
splitOn delim str = case break (== head delim) str of
  (pre, "") -> [pre]
  (pre, suf) -> pre : splitOn delim (tail suf)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
