
import System.IO

data Coord = Coord { x :: Int, y :: Int } deriving (Show)

add :: Coord -> Coord -> Coord
add c1 c2 = Coord (x c1 + x c2) (y c1 + y c2)

multiplyByScalar :: Coord -> Int -> Coord
multiplyByScalar c s = Coord (x c * s) (y c * s)

north = Coord 0 (-1)
west = Coord (-1) 0
south = Coord 0 1
east = Coord 1 0

abs' :: Int -> Int
abs' x = if x < 0 then -x else x

parseInput :: [String] -> [Coord]
parseInput input = reverse $ foldl processLine [Coord 0 0] input
  where
    processLine :: [Coord] -> String -> [Coord]
    processLine (current:rest) line = 
      let parts = words line
          color = parts !! 2
          dirInput = color !! 7
          lengthStr = take 5 $ drop 2 color
          length = hexStringToInt lengthStr
          dir = case dirInput of
            '3' -> north
            '2' -> west
            '1' -> south
            '0' -> east
      in (current `add` (dir `multiplyByScalar` length)) : current : rest

hexStringToInt :: String -> Int
hexStringToInt hexStr = read $ "0x" ++ hexStr

shoelace :: [Coord] -> Int
shoelace vertices = abs' $ div (foldl calculateArea 0 $ zip vertices (tail $ cycle vertices)) 2
  where
    calculateArea :: Int -> (Coord, Coord) -> Int
    calculateArea acc (c1, c2) = x c1 * y c2 - y c1 * x c2 + acc

perimeter :: [Coord] -> Int
perimeter vertices = div (foldl calculatePerimeter 0 $ zip vertices (tail $ cycle vertices)) 2
  where
    calculatePerimeter :: Int -> (Coord, Coord) -> Int
    calculatePerimeter acc (c1, c2) = abs' (x c1 - x c2) + abs' (y c1 - y c2) + acc

calculatePolygonArea :: [Coord] -> Int
calculatePolygonArea vertices = shoelace vertices + perimeter vertices + 1

solve :: [String] -> Int
solve input = calculatePolygonArea $ parseInput input

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print $ solve input
