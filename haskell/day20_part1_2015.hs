
import Data.List (find)

-- Efficiently calculate the sum of divisors for a given number
sumOfDivisors :: Int -> Int
sumOfDivisors n = foldl (\acc x -> if n `mod` x == 0 then acc + x + (if x * x /= n then n `div` x else 0) else acc) 0 [1..limit]
  where
    limit = floor . sqrt $ fromIntegral n

-- Calculate the presents delivered to a house
presentsAtHouse :: Int -> Int
presentsAtHouse house = 10 * sumOfDivisors house

-- Find the first house with at least the target number of presents
findHouse :: Int -> Int
findHouse target = fst . head $ dropWhile (\(_, presents) -> presents < target) [(house, presentsAtHouse house) | house <- [1..]]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let target = read input :: Int
  print $ findHouse target
