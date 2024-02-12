import Data.List

parseDirections :: String -> [(Int, Int)]
parseDirections = map parseDirection

parseDirection :: Char -> (Int, Int)
parseDirection '^' = (0, 1)
parseDirection 'v' = (0, -1)
parseDirection '>' = (1, 0)
parseDirection '<' = (-1, 0)
parseDirection _ = (0, 0)

move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (x, y) (dx, dy) = (x + dx, y + dy)

visitedHouses :: [(Int, Int)] -> [(Int, Int)]
visitedHouses directions = scanl move (0, 0) directions

main = do
    contents <- readFile "input.txt"
    let directions = parseDirections contents
    let houses = visitedHouses directions
    print $ length $ nub houses