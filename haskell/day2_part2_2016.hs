
import Data.Char (digitToInt)
import Data.List (foldl')

type Keypad1 = [[Char]]
type Keypad2 = [[Char]]
type Position = (Int, Int)

keypad1 :: Keypad1
keypad1 = ["123", "456", "789"]

keypad2 :: Keypad2
keypad2 = ["  1  ", " 234 ", "56789", " ABC ", "  D  "]

move :: Keypad1 -> Position -> Char -> Position
move keypad (row, col) 'U' = if row > 0 && keypad !! (row - 1) !! col /= ' ' then (row - 1, col) else (row, col)
move keypad (row, col) 'D' = if row < length keypad - 1 && keypad !! (row + 1) !! col /= ' ' then (row + 1, col) else (row, col)
move keypad (row, col) 'L' = if col > 0 && keypad !! row !! (col - 1) /= ' ' then (row, col - 1) else (row, col)
move keypad (row, col) 'R' = if col < length (head keypad) - 1 && keypad !! row !! (col + 1) /= ' ' then (row, col + 1) else (row, col)
move _ pos _ = pos

findStartPos :: Keypad1 -> Char -> Position
findStartPos keypad start = head [(r, c) | r <- [0..length keypad - 1], c <- [0..length (head keypad) - 1], keypad !! r !! c == start]

findStartPos2 :: Keypad2 -> Char -> Position
findStartPos2 keypad start = head [(r, c) | r <- [0..length keypad - 1], c <- [0..length (head keypad) - 1], keypad !! r !! c == start]

solve :: Keypad1 -> String -> String
solve keypad input =
  let startPos = findStartPos keypad '5'
      code = foldl' (\(pos, acc) line ->
                      let newPos = foldl' (move keypad) pos line
                      in (newPos, acc ++ [keypad !! (fst newPos) !! (snd newPos)])
                    ) (startPos, "") (lines input)
  in snd code

solve2 :: Keypad2 -> String -> String
solve2 keypad input =
  let startPos = findStartPos2 keypad '5'
      code = foldl' (\(pos, acc) line ->
                      let newPos = foldl' (move keypad) pos line
                      in (newPos, acc ++ [keypad !! (fst newPos) !! (snd newPos)])
                    ) (startPos, "") (lines input)
  in snd code

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ "Part 1: " ++ solve keypad1 input
  putStrLn $ "Part 2: " ++ solve2 keypad2 input
