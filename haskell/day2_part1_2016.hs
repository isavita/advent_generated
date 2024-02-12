
import System.IO

type Keypad = [[Int]]
type Position = (Int, Int)

keypad :: Keypad
keypad = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

move :: Position -> Char -> Position
move (x, y) 'U' = if x > 0 then (x - 1, y) else (x, y)
move (x, y) 'D' = if x < 2 then (x + 1, y) else (x, y)
move (x, y) 'L' = if y > 0 then (x, y - 1) else (x, y)
move (x, y) 'R' = if y < 2 then (x, y + 1) else (x, y)

findCode :: Keypad -> Position -> [String] -> String
findCode _ _ [] = []
findCode keypad pos (instr:instrs) = let newPos = foldl move pos instr
                                      in show (keypad !! fst newPos !! snd newPos) ++ findCode keypad newPos instrs

main = do
    contents <- readFile "input.txt"
    let instructions = lines contents
    putStrLn $ findCode keypad (1, 1) instructions
