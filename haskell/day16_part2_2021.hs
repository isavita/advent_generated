
import Data.Bits
import Data.Char
import Data.List
import System.IO

hexToBin :: String -> String
hexToBin = concatMap (pad . toBin . digitToInt)
  where
    toBin 0 = ""
    toBin n = toBin (n `div` 2) ++ show (n `mod` 2)
    pad s = replicate (4 - length s) '0' ++ s

parsePacket :: String -> Int -> (Int, Int, Integer)
parsePacket s idx =
    let version = foldl' (\a b -> a * 2 + fromEnum (b == '1')) 0 (take 3 $ drop idx s)
        typeID  = foldl' (\a b -> a * 2 + fromEnum (b == '1')) 0 (take 3 $ drop (idx + 3) s)
        idx'    = idx + 6
    in if typeID == 4
       then let (v, i) = readLiteral s idx'
            in (version, i, v)
       else let (lenType, idx'') = (s !! idx', idx' + 1)
                (subPacketLength, numSubPackets, idx''') =
                    if lenType == '0'
                    then let l = foldl' (\a b -> a * 2 + fromEnum (b == '1')) 0 (take 15 $ drop idx'' s)
                         in (l, -1, idx'' + 15)
                    else let n = foldl' (\a b -> a * 2 + fromEnum (b == '1')) 0 (take 11 $ drop idx'' s)
                         in (-1, n, idx'' + 11)
                (values, idx'''') = parseSubPackets s idx''' lenType subPacketLength numSubPackets []
                result = case typeID of
                           0 -> sum values
                           1 -> product values
                           2 -> minimum values
                           3 -> maximum values
                           5 -> if head values > values !! 1 then 1 else 0
                           6 -> if head values < values !! 1 then 1 else 0
                           7 -> if head values == values !! 1 then 1 else 0
                           _ -> error "Unknown typeID"
            in (version, idx'''', result)

readLiteral :: String -> Int -> (Integer, Int)
readLiteral s idx = go 0 idx
  where
    go acc i
        | s !! i == '1' =
            let nibble = foldl' (\a b -> a * 2 + fromEnum (b == '1')) 0 (take 4 $ drop (i + 1) s)
            in go (acc * 16 + fromIntegral nibble) (i + 5)
        | otherwise =
            let nibble = foldl' (\a b -> a * 2 + fromEnum (b == '1')) 0 (take 4 $ drop (i + 1) s)
            in (acc * 16 + fromIntegral nibble, i + 5)

parseSubPackets :: String -> Int -> Char -> Int -> Int -> [Integer] -> ([Integer], Int)
parseSubPackets s idx lenType lenLeft numLeft acc
    | lenType == '0' && lenLeft <= 0 = (reverse acc, idx)
    | lenType == '1' && numLeft <= 0 = (reverse acc, idx)
    | otherwise =
        let (_, newIdx, val) = parsePacket s idx
            newLenLeft = lenLeft - (newIdx - idx)
            newNumLeft = numLeft - 1
        in parseSubPackets s newIdx lenType newLenLeft newNumLeft (val : acc)

main :: IO ()
main = do
    h <- openFile "input.txt" ReadMode
    hexStr <- hGetLine h
    hClose h
    let binStr = hexToBin hexStr
        (_, _, value) = parsePacket binStr 0
    print value
