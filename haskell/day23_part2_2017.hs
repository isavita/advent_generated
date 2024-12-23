
main = do
  let b = 57 * 100 + 100000
  let c = b + 17000
  let h = length [x | x <- [b, b + 17 .. c], not (isPrime x)]
  print h

isPrime :: Int -> Bool
isPrime n = all (\i -> n `mod` i /= 0) [2 .. floor (sqrt (fromIntegral n))]
