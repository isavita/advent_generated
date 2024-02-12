
import System.IO
import Data.List

processLine :: String -> Int
processLine = read

calcFuelMass :: Int -> Int
calcFuelMass mass
  | fuel <= 0 = 0
  | otherwise = fuel + calcFuelMass fuel
  where fuel = (mass `div` 3) - 2

getTotal :: [Int] -> Int
getTotal = sum . map calcFuelMass

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let masses = map processLine $ lines contents
      total = getTotal masses
  print total
  hClose handle
