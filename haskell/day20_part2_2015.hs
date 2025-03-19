
import Data.Array
import Data.List

main :: IO ()
main = do
  target <- fmap (flip div 11 . read) $ readFile "input.txt"
  let houses = accumArray (+) 0 (1, target) $ concatMap (\elf -> map (\house -> (house, elf)) [elf, elf*2..min (elf*50) target]) [1..target]
  print $ head $ filter ((>= target) . (houses !)) [1..target]
