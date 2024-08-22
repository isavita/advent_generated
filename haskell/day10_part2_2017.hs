import Data.Bits (xor)
import Data.Char (ord)
import Data.List (foldl')
import Numeric (showHex)
import System.IO (readFile)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let lengths = map ord input ++ [17, 31, 73, 47, 23]
        sparseHash = knotHash [0..255] lengths 0 0 64
        denseHash = [foldl' xor 0 (take 16 (drop (i * 16) sparseHash)) | i <- [0..15]]
        hexString = concatMap (\x -> if length (showHex x "") == 1 then '0' : showHex x "" else showHex x "") denseHash
    putStrLn hexString

knotHash :: [Int] -> [Int] -> Int -> Int -> Int -> [Int]
knotHash list lengths pos skip rounds
    | rounds == 0 = list
    | otherwise = knotHash newList lengths newPos newSkip (rounds - 1)
    where
        (newList, newPos, newSkip) = foldl' (\(lst, p, s) len -> (reverseSection lst p len, (p + len + s) `mod` length lst, s + 1)) (list, pos, skip) lengths

reverseSection :: [Int] -> Int -> Int -> [Int]
reverseSection list pos len = foldl' (\lst i -> let j = (pos + i) `mod` length list in replace lst j (list !! ((pos + len - 1 - i) `mod` length list))) list [0..len-1]

replace :: [a] -> Int -> a -> [a]
replace list index newElem = take index list ++ [newElem] ++ drop (index + 1) list