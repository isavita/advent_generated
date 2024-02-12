
import Data.List

type Position = (Int, Int)
data Direction = North | East | South | West deriving (Enum)

turn :: Direction -> Char -> Direction
turn dir 'R' = toEnum $ (fromEnum dir + 1) `mod` 4
turn dir 'L' = toEnum $ (fromEnum dir - 1) `mod` 4

move :: Position -> Direction -> Int -> Position
move (x, y) North dist = (x, y + dist)
move (x, y) East dist = (x + dist, y)
move (x, y) South dist = (x, y - dist)
move (x, y) West dist = (x - dist, y)

parseInstruction :: String -> (Char, Int)
parseInstruction (dir:dist) = (dir, read dist)

applyInstruction :: (Position, Direction) -> (Char, Int) -> (Position, Direction)
applyInstruction ((x, y), dir) (turnDir, dist) = (move (x, y) newDir dist, newDir)
    where newDir = turn dir turnDir

main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = map parseInstruction $ words $ map (\c -> if c == ',' then ' ' else c) input
        finalPos = foldl applyInstruction ((0, 0), North) instructions
        distance = abs (fst (fst finalPos)) + abs (snd (fst finalPos))
    print distance
