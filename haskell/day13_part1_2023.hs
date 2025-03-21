
import System.IO
import Data.Bits
import Data.List
import Data.Maybe (fromMaybe)

parseMirror :: [String] -> ([Int], [Int])
parseMirror mirrorStr = (rows, cols)
  where
    rows = map rowToInt mirrorStr
    cols = map colToInt [0 .. (length (head mirrorStr) - 1)]
    rowToInt line = foldl (\acc c -> (acc `shiftL` 1) + (if c == '#' then 1 else 0)) 0 line
    colToInt x = foldl (\acc line -> (acc `shiftL` 1) + (if line !! x == '#' then 1 else 0)) 0 mirrorStr

getMirrorAxis :: [Int] -> Int
getMirrorAxis lines = fromMaybe 0 $ find (\i -> all (\j -> lines !! (i - 1 - j) == lines !! (i + j)) [0 .. min (i - 1) (length lines - i - 1)]) [1 .. (length lines - 1)]

getMirrorAxisWithOneSmudge :: [Int] -> Int
getMirrorAxisWithOneSmudge lines = fromMaybe 0 $ find (\i -> (== 1) $ sum $ map (\j -> bitDiff (lines !! (i - 1 - j)) (lines !! (i + j))) [0 .. min (i - 1) (length lines - i - 1)]) [1 .. (length lines - 1)]
  where
    bitDiff x y = if (xor x y) .&. ((xor x y) - 1) == 0 then if x == y then 0 else 1 else 0

solve :: [String] -> Int
solve input = sum $ map processMirror $ splitMirrors input
  where
    splitMirrors :: [String] -> [[String]]
    splitMirrors [] = []
    splitMirrors input = mirror : splitMirrors rest
      where
        (mirrorLines, restWithNewline) = break (== "") input
        mirror = mirrorLines
        rest = drop 1 restWithNewline

    processMirror :: [String] -> Int
    processMirror mirrorStr = cols + rows * 100
      where
        (rowsLines, colsLines) = parseMirror mirrorStr
        cols = getMirrorAxis colsLines
        rows = getMirrorAxis rowsLines

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let inputLines = lines contents
  print $ solve inputLines
