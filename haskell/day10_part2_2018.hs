
import Data.List
import Data.Char

data Star = Star { x :: Int, y :: Int, vX :: Int, vY :: Int } deriving Show

main = do
    contents <- readFile "input.txt"
    let ls = lines contents
    let stars = map parseStar ls
    let (smallestT, _) = minimumBy (\(_, a) (_, b) -> compare a b) [(t, area t stars) | t <- [1..100000]]
    print smallestT

parseStar :: String -> Star
parseStar line = Star x y vX vY
    where [x, y, vX, vY] = map readInts $ words $ filter (\c -> isDigit c || c == '-' || c == ' ') line

readInts :: String -> Int
readInts = read . filter (\c -> isDigit c || c == '-')

area :: Int -> [Star] -> Int
area t stars = (maxX - minX + 1) + (maxY - minY + 1)
    where positions = map (\(Star x y vX vY) -> (x + vX * t, y + vY * t)) stars
          (maxX, maxY) = foldl' (\(mx, my) (x, y) -> (max mx x, max my y)) (0, 0) positions
          (minX, minY) = foldl' (\(mnx, mny) (x, y) -> (min mnx x, min mny y)) (0, 0) positions
