
module Main where

import System.IO

josephus :: Int -> Int
josephus n = n - i + max (n - 2 * i) 0
  where
    i = last $ takeWhile (<= n) $ iterate (* 3) 1

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let numElves = read contents :: Int
  print (josephus numElves)
  hClose handle
