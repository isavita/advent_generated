
import System.IO
import Data.List (sortOn)

contains :: [(Int,Int)] -> Int -> Bool
contains rs x = go 0 (length rs)
  where
    go lo hi
      | lo >= hi = False
      | otherwise =
          let mid = (lo + hi) `div` 2
              (a,b) = rs !! mid
          in if x < a then go lo mid
             else if x > b then go (mid+1) hi
             else True

mergeSorted :: [(Int,Int)] -> [(Int,Int)]
mergeSorted [] = []
mergeSorted (p:ps) = go p ps
  where
    go cur [] = [cur]
    go (a,b) ((c,d):rest)
      | c > b     = (a,b) : go (c,d) rest
      | otherwise = go (a, max b d) rest

main :: IO ()
main = do
    content <- readFile "input.txt"
    let ls = lines content
        (rangeLines, _:idLines) = break null ls
        ranges = [ (read a, read b)
                 | line <- rangeLines
                 , let (a:b:_) = words $ map (\c -> if c == '-' then ' ' else c) line
                 ]
        ids = map read idLines
        merged = mergeSorted $ sortOn fst ranges
        fresh = length $ filter (contains merged) ids
    putStrLn $ "Number of fresh ingredients: " ++ show fresh
