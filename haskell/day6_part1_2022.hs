
import Data.List (nub)
import System.IO (readFile)

-- Function to find the first position of the start-of-packet marker
findStartOfPacketMarker :: String -> Int
findStartOfPacketMarker stream = go 0 stream
  where
    go index xs
      | length xs < 4 = -1  -- Not enough characters
      | length (nub (take 4 xs)) == 4 = index + 4  -- Found the marker
      | otherwise = go (index + 1) (tail xs)  -- Move to the next character

main :: IO ()
main = do
    content <- readFile "input.txt"
    let result = findStartOfPacketMarker (filter (/= '\n') content)
    print result
