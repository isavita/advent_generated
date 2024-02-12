
import Data.Bits

main = do
    contents <- readFile "input.txt"
    let maxSeatID = maximum $ map (decode . map replaceChars) (lines contents)
    print maxSeatID

replaceChars :: Char -> Char
replaceChars 'F' = '0'
replaceChars 'B' = '1'
replaceChars 'L' = '0'
replaceChars 'R' = '1'

decode :: String -> Int
decode pass = row * 8 + column
    where
        row = binaryToInt (take 7 pass)
        column = binaryToInt (drop 7 pass)

binaryToInt :: String -> Int
binaryToInt binaryStr = foldl (\acc x -> shiftL acc 1 .|. digitToInt x) 0 binaryStr
    where
        digitToInt '0' = 0
        digitToInt '1' = 1
