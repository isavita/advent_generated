import System.IO
import Data.List
import qualified Data.Set as Set

data Direction = North | East | South | West deriving (Enum, Show)

turn :: Direction -> Char -> Direction
turn dir 'L' = toEnum $ (fromEnum dir + 3) `mod` 4
turn dir 'R' = toEnum $ (fromEnum dir + 1) `mod` 4

move :: (Int, Int) -> Direction -> Int -> (Int, Int)
move (x, y) North dist = (x, y + dist)
move (x, y) East dist  = (x + dist, y)
move (x, y) South dist = (x, y - dist)
move (x, y) West dist  = (x - dist, y)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let instructions = words $ map (\c -> if c == ',' then ' ' else c) contents
        visited = Set.singleton (0, 0)
        (_, _, _, firstRepeated) = foldl' processInstruction ((0, 0), North, visited, Nothing) instructions
    case firstRepeated of
        Just (x, y) -> print $ abs x + abs y
        Nothing -> putStrLn "No location visited twice"

processInstruction :: ((Int, Int), Direction, Set.Set (Int, Int), Maybe (Int, Int)) -> String -> ((Int, Int), Direction, Set.Set (Int, Int), Maybe (Int, Int))
processInstruction ((x, y), dir, visited, firstRepeated) instruction
    | Just loc <- firstRepeated = ((x, y), dir, visited, Just loc)
    | otherwise = foldl' visit ((x, y), newDir, visited, Nothing) [1..dist]
  where
    (turnDir:distStr) = instruction
    dist = read distStr
    newDir = turn dir turnDir

visit :: ((Int, Int), Direction, Set.Set (Int, Int), Maybe (Int, Int)) -> Int -> ((Int, Int), Direction, Set.Set (Int, Int), Maybe (Int, Int))
visit ((x, y), dir, visited, firstRepeated) _ = 
    let newPos = move (x, y) dir 1
    in if Set.member newPos visited
       then (newPos, dir, visited, Just newPos)
       else (newPos, dir, Set.insert newPos visited, firstRepeated)