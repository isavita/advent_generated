
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

type Grid = [String]
type Direction = (Int, Int)

allDirections :: [Direction]
allDirections = [(0, 1), (1, 0), (1, 1), (-1, 1), (0, -1), (-1, 0), (-1, -1), (1, -1)]

checkWord :: Grid -> String -> Int -> Int -> Direction -> Bool
checkWord grid word x y (dx, dy) = all checkIndex $ zip [0..] word
  where
    checkIndex (i, c) =
      let nx = x + dx * i
          ny = y + dy * i
      in  nx >= 0 && nx < length grid &&
          ny >= 0 && ny < length (head grid) &&
          grid !! nx !! ny == c

countOccurrences :: Grid -> String -> Int
countOccurrences grid word = sum $ map (countFromPos grid word) $ concatMap (\x -> map (\y -> (x,y)) [0..length (head grid) -1]) [0..length grid -1]

countFromPos :: Grid -> String -> (Int, Int) -> Int
countFromPos grid word (x, y) = length $ filter (checkWord grid word x y) allDirections

main :: IO ()
main = do
  contents <- BS.readFile "input.txt"
  let grid = filter (not . null) $ map BS.unpack $ BS.lines contents
  let count = countOccurrences grid "XMAS"
  putStrLn $ "XMAS appears " ++ show count ++ " times in the word search"
