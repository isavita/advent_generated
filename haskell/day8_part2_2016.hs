
import Data.Array
import System.IO
import Data.List (foldl')
import Text.ParserCombinators.ReadP
import Control.Monad (forM_)

-- Constants
rows, cols :: Int
rows = 6
cols = 50

-- Data Types
type Screen = Array (Int, Int) Char
data Instruction = Rect Int Int | RotateRow Int Int | RotateCol Int Int deriving (Show)

-- Parsing using ReadP for robustness
parseNum :: ReadP Int
parseNum = readS_to_P reads

parseRect :: ReadP Instruction
parseRect = do
    _ <- string "rect "
    w <- parseNum
    _ <- char 'x'
    h <- parseNum
    return $ Rect w h

parseRotate :: ReadP Instruction
parseRotate = do
    _ <- string "rotate "
    dir <- choice [string "row y=", string "column x="]
    idx <- parseNum
    _ <- string " by "
    by <- parseNum
    return $ if take 3 dir == "row" then RotateRow idx by else RotateCol idx by

parseInstruction :: ReadP Instruction
parseInstruction = choice [parseRect, parseRotate] <* eof

runParser :: String -> Instruction
runParser s = case readP_to_S parseInstruction s of
    [(instr, "")] -> instr
    _             -> error $ "Failed to parse instruction: " ++ s

-- Initial State
initialScreen :: Screen
initialScreen = listArray ((0, 0), (rows - 1, cols - 1)) (repeat '.')

-- Rotation Helper
rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = let len = length xs; n' = n `mod` len in drop (len - n') xs ++ take (len - n') xs

-- Operations
applyInstruction :: Screen -> Instruction -> Screen
applyInstruction screen (Rect w h) =
    screen // [((r, c), '#') | r <- [0..h-1], c <- [0..w-1]]
applyInstruction screen (RotateRow r by) =
    let currentRow = [screen ! (r, c) | c <- [0..cols-1]]
        rotatedRow = rotateList by currentRow
        updates = [((r, c), rotatedRow !! c) | c <- [0..cols-1]]
    in screen // updates
applyInstruction screen (RotateCol c by) =
    let currentCol = [screen ! (r, c) | r <- [0..rows-1]]
        rotatedCol = rotateList by currentCol
        updates = [((r, c), rotatedCol !! r) | r <- [0..rows-1]]
    in screen // updates

-- Printing
printScreen :: Screen -> IO ()
printScreen screen =
    forM_ [0..rows-1] $ \r ->
        putStrLn [screen ! (r, c) | c <- [0..cols-1]]

-- Main
main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = map runParser $ lines input
    let finalScreen = foldl' applyInstruction initialScreen instructions
    printScreen finalScreen

    -- To print the count of lit pixels (often part of such problems)
    -- let litPixels = length $ filter (== '#') $ elems finalScreen
    -- putStrLn $ "Lit pixels: " ++ show litPixels
