
import System.IO (readFile)
import Data.Char (isLower, toLower)
import Data.List (foldl')

-- Function to react the polymer
reactPolymer :: String -> String
reactPolymer = foldl' react []
  where
    react [] x = [x]
    react (y:ys) x
      | toLower y == toLower x && (isLower y /= isLower x) = ys  -- React
      | otherwise = x:y:ys  -- No reaction, keep both

-- Main function to read input and print the result
main :: IO ()
main = do
    input <- readFile "input.txt"
    let reactedPolymer = reactPolymer (filter (/= '\n') input)  -- Remove newlines
    print (length reactedPolymer)  -- Output the length of the resulting polymer
