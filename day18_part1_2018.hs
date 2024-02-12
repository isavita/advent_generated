import Data.List

type Grid = [[Char]]

getAdjacents :: Grid -> Int -> Int -> [Char]
getAdjacents grid x y = [grid !! i !! j | i <- [x-1..x+1], j <- [y-1..y+1], i /= x || j /= y, i >= 0, j >= 0, i < length grid, j < length (head grid)]

applyRules :: Char -> [Char] -> Char
applyRules '.' adj = if length (filter (== '|') adj) >= 3 then '|' else '.'
applyRules '|' adj = if length (filter (== '#') adj) >= 3 then '#' else '|'
applyRules '#' adj = if '|' `elem` adj && '#' `elem` adj then '#' else '.'

updateGrid :: Grid -> Grid
updateGrid grid = [[applyRules (grid !! i !! j) (getAdjacents grid i j) | j <- [0..length (head grid) - 1]] | i <- [0..length grid - 1]]

countResources :: Grid -> Int
countResources grid = length (filter (== '|') (concat grid)) * length (filter (== '#') (concat grid))

main = do
    contents <- readFile "input.txt"
    let grid = lines contents
    let finalGrid = iterate updateGrid grid !! 10
    print $ countResources finalGrid