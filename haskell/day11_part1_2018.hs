
import Data.Char (digitToInt)
import Data.List (maximumBy)
import Data.Ord (comparing)

powerLevel :: Int -> Int -> Int -> Int
powerLevel x y serial =
  let rackId = x + 10
      level = rackId * y
      level' = (level + serial) * rackId
      hundredsDigit = (level' `div` 100) `mod` 10
   in hundredsDigit - 5

grid :: Int -> [[Int]]
grid serial = [[powerLevel x y serial | x <- [1..300]] | y <- [1..300]]

squarePower :: [[Int]] -> Int -> Int -> Int
squarePower g x y = sum $ do
  dx <- [0..2]
  dy <- [0..2]
  return $ (g !! (y + dy - 1)) !! (x + dx - 1)

findMaxSquare :: [[Int]] -> (Int, Int, Int)
findMaxSquare g = maximumBy (comparing (\(_, _, p) -> p)) $ do
  x <- [1..298]
  y <- [1..298]
  let power = squarePower g x y
  return (x, y, power)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let serial = read $ head $ lines contents :: Int
  let g = grid serial
  let (x, y, _) = findMaxSquare g
  print $ show x ++ "," ++ show y
