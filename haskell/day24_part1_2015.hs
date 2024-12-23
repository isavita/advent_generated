
import Data.List
import Data.Maybe
import System.IO

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let packages = map read $ lines contents :: [Int]
  let totalWeight = sum packages
  let targetWeight = totalWeight `div` 3
  let validGroups = filter ((== targetWeight) . sum) $ subsequences packages
  let bestQE = minimum $ map product $ filter ((==) (minimum $ map length validGroups) . length) validGroups
  print bestQE
