import System.IO

data Direction = Up | Down | Lft | Rght deriving (Eq)

main = do
    contents <- readFile "input.txt"
    let grid = lines contents
        startX = head [x | (x, c) <- zip [0..] (head grid), c == '|']
        (letters, _, _, _) = followPath grid startX 0 Down [] 0
    putStrLn letters

followPath :: [String] -> Int -> Int -> Direction -> String -> Int -> (String, Int, Int, Direction)
followPath grid x y dir letters steps
    | current == ' ' = (letters, x, y, dir)
    | current `elem` ['|', '-'] || (current >= 'A' && current <= 'Z') = move grid x y dir letters steps
    | current == '+' = turn grid x y dir letters steps
    where current = (grid !! y) !! x

move :: [String] -> Int -> Int -> Direction -> String -> Int -> (String, Int, Int, Direction)
move grid x y dir letters steps
    | dir == Up = followPath grid x (y-1) dir (collectLetter grid x y letters) (steps+1)
    | dir == Down = followPath grid x (y+1) dir (collectLetter grid x y letters) (steps+1)
    | dir == Lft = followPath grid (x-1) y dir (collectLetter grid x y letters) (steps+1)
    | dir == Rght = followPath grid (x+1) y dir (collectLetter grid x y letters) (steps+1)

turn :: [String] -> Int -> Int -> Direction -> String -> Int -> (String, Int, Int, Direction)
turn grid x y dir letters steps
    | dir == Up || dir == Down = if (grid !! y) !! (x-1) == '-' then move grid (x-1) y Lft letters steps else move grid (x+1) y Rght letters steps
    | dir == Lft || dir == Rght = if (grid !! (y-1)) !! x == '|' then move grid x (y-1) Up letters steps else move grid x (y+1) Down letters steps

collectLetter :: [String] -> Int -> Int -> String -> String
collectLetter grid x y letters
    | current >= 'A' && current <= 'Z' = letters ++ [current]
    | otherwise = letters
    where current = (grid !! y) !! x