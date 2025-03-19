
module Main where

import qualified Data.Map as M

process :: [(Int, Int)] -> Int
process pairs = sum $ map calculate totals
  where
    counts = foldr (\(_, sim) acc -> M.insertWith (+) sim 1 acc) M.empty pairs
    totals = map (\(loc, sim) -> (loc, M.findWithDefault 0 loc counts)) pairs
    calculate (locId, count) = locId * count
   

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let input = lines contents
  let pairs = map ((\[x, y] -> (read x :: Int, read y :: Int)) . words) input
  print $ process pairs
