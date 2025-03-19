
main :: IO ()
main = do
  input <- readFile "input.txt"
  let pairs = lines input
      (locations1, locations2) = unzip $ map ((\[x, y] -> (read x, read y)) . words) pairs
      sorted1 = sort locations1
      sorted2 = sort locations2
      distances = zipWith (\x y -> abs (x - y)) sorted1 sorted2
  print (sum distances)

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = lesser ++ [x] ++ greater
  where
    lesser  = sort [a | a <- xs, a <= x]
    greater = sort [a | a <- xs, a > x]
