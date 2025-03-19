
import Data.Bits
import Data.List

parseMirror :: [String] -> ([Int], [Int])
parseMirror mirrorStr = (rows, cols)
  where
    rows = map rowToInt mirrorStr
    cols = map colToInt $ transpose mirrorStr
    rowToInt row = foldl' (\acc c -> (acc `shiftL` 1) .|. if c == '#' then 1 else 0) 0 row
    colToInt col = foldl' (\acc c -> (acc `shiftL` 1) .|. if c == '#' then 1 else 0) 0 col

getMirrorAxis :: [Int] -> Int
getMirrorAxis lines = head $ [i | i <- [1..length lines - 1], isMirror i] ++ [0]
  where
    isMirror i = all (\j -> lines !! (i-1-j) == lines !! (i+j)) [0..min (i-1) (length lines - i -1)]

getMirrorAxisWithSmudge :: [Int] -> Int
getMirrorAxisWithSmudge lines = head $ [i | i <- [1..length lines - 1], isMirror i] ++ [0]
  where
    isMirror i = (== 1) . sum $ [countBits (lines !! (i-1-j) `xor` lines !! (i+j)) | j <- [0..min (i-1) (length lines - i -1)]]
    countBits 0 = 0
    countBits x = (x .&. 1) + countBits (x `shiftR` 1)

solve :: [String] -> Int
solve inputLines = sum [getMirrorAxisWithSmudge cols + 100 * getMirrorAxisWithSmudge rows | (rows, cols) <- mirrors]
  where
    mirrors = map parseMirror (splitMirrors inputLines)
    splitMirrors [] = []
    splitMirrors lines = firstMirror : splitMirrors (drop 1 restLines)
      where
        (firstMirror, restLines) = break null lines

main :: IO ()
main = do
  inputLines <- readFile "input.txt"
  print $ solve (lines (init inputLines))
