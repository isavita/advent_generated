
import System.IO

data Ship = Ship { x :: Int, y :: Int, facing :: Int }

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let instructions = lines contents
        ship = Ship 0 0 0
        finalShip = foldl processInstruction ship instructions
        manhattanDistance = Prelude.abs (x finalShip) + Prelude.abs (y finalShip)
    print manhattanDistance
    hClose handle

processInstruction :: Ship -> String -> Ship
processInstruction ship instruction = case (head instruction, read (tail instruction) :: Int) of
    ('N', value) -> ship { y = y ship + value }
    ('S', value) -> ship { y = y ship - value }
    ('E', value) -> ship { x = x ship + value }
    ('W', value) -> ship { x = x ship - value }
    ('L', value) -> ship { facing = (facing ship - value + 360) `mod` 360 }
    ('R', value) -> ship { facing = (facing ship + value) `mod` 360 }
    ('F', value) -> case facing ship of
        0   -> ship { x = x ship + value }
        90  -> ship { y = y ship - value }
        180 -> ship { x = x ship - value }
        270 -> ship { y = y ship + value }
    _ -> ship

