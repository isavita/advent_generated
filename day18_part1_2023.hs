
data Coord = Coord { x :: Int, y :: Int }

add :: Coord -> Coord -> Coord
add (Coord x1 y1) (Coord x2 y2) = Coord (x1 + x2) (y1 + y2)

multiplyByScalar :: Coord -> Int -> Coord
multiplyByScalar (Coord x y) s = Coord (x * s) (y * s)

north = Coord 0 (-1)
west = Coord (-1) 0
south = Coord 0 1
east = Coord 1 0

abs' :: Int -> Int
abs' x = if x < 0 then -x else x

parseInput :: [String] -> [Coord]
parseInput = foldl processInput [Coord 0 0]
  where
    processInput :: [Coord] -> String -> [Coord]
    processInput (current:vertices) line =
      let parts = words line
          dirInput = head (head parts)
          lengthStr = parts !! 1
          length = read lengthStr
          dir = case dirInput of
            'U' -> north
            'L' -> west
            'D' -> south
            'R' -> east
      in current `add` (dir `multiplyByScalar` length) : current : vertices

shoelace :: [Coord] -> Int
shoelace vertices = abs' area `div` 2
  where
    n = length vertices
    area = sum [x1 * y2 - y1 * x2 | i <- [0..n-1], let next = (i + 1) `mod` n, let Coord x1 y1 = vertices !! i, let Coord x2 y2 = vertices !! next]

perimeter :: [Coord] -> Int
perimeter vertices = sum [abs' (x1 - x2) + abs' (y1 - y2) | i <- [0..n-1], let next = (i + 1) `mod` n, let Coord x1 y1 = vertices !! i, let Coord x2 y2 = vertices !! next]
  where
    n = length vertices

calculatePolygonArea :: [Coord] -> Int
calculatePolygonArea vertices = shoelace vertices + perimeter vertices `div` 2 + 1

solve :: [String] -> Int
solve input = calculatePolygonArea (parseInput input)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print (solve input)
