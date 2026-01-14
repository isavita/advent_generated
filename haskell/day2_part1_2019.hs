
{-# LANGUAGE BangPatterns #-}

import Data.List (unfoldr)
import Data.Char (isDigit)

main :: IO ()
main = do
  s <- readFile "input.txt"
  let xs = map read (split (==',') s)
      xs' = xs // [(1,12),(2,2)]
  print (run xs')

run :: [Int] -> Int
run xs = go xs 0
  where
    go !m !p
      | m!!p == 99 = m!!0
      | op == 1    = go (m // [(m!!(p+3), m!!(m!!(p+1)) + m!!(m!!(p+2)))]) (p+4)
      | op == 2    = go (m // [(m!!(p+3), m!!(m!!(p+1)) * m!!(m!!(p+2)))]) (p+4)
      | otherwise  = error "bad op"
      where op = m!!p

(//) :: [a] -> [(Int,a)] -> [a]
(//) m ps = foldr (\(i,x) a -> take i a ++ x : drop (i+1) a) m ps

split :: (Char -> Bool) -> String -> [String]
split p = unfoldr f
  where f [] = Nothing
        f xs = Just (let (a,b) = break p xs in (a, dropWhile p b))
