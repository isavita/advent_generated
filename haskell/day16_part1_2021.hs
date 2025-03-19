
import Data.Char (digitToInt)
import Data.Bits

hexToBin :: String -> String
hexToBin = concatMap hexDigitToBin

hexDigitToBin :: Char -> String
hexDigitToBin h = case digitToInt h of
  0 -> "0000"
  1 -> "0001"
  2 -> "0010"
  3 -> "0011"
  4 -> "0100"
  5 -> "0101"
  6 -> "0110"
  7 -> "0111"
  8 -> "1000"
  9 -> "1001"
  10 -> "1010"
  11 -> "1011"
  12 -> "1100"
  13 -> "1101"
  14 -> "1110"
  15 -> "1111"
  _ -> ""

binToInt :: String -> Int
binToInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

parsePacket :: String -> Int -> (Int, Int)
parsePacket binStr idx =
  let version = binToInt (take 3 (drop idx binStr))
      typeID = binToInt (take 3 (drop (idx + 3) binStr))
      idx' = idx + 6
  in if typeID == 4
     then
        let (newIdx, _) = parseLiteral (drop idx' binStr) idx'
        in (version, newIdx)

     else
        let lengthTypeID = digitToInt (binStr !! idx')
            idx'' = idx' + 1
        in if lengthTypeID == 0
           then
              let subPacketLength = binToInt (take 15 (drop idx'' binStr))
                  idx''' = idx'' + 15
                  (versionSum, newIdx) = parseSubPackets0 binStr idx''' subPacketLength version
              in (versionSum, newIdx)
           else
              let numSubPackets = binToInt (take 11 (drop idx'' binStr))
                  idx''' = idx'' + 11
                  (versionSum, newIdx) = parseSubPackets1 binStr idx''' numSubPackets version
              in (versionSum,newIdx)
    where
        parseLiteral :: String -> Int ->(Int,String)
        parseLiteral str currentIdx =
            if head str == '1'
            then parseLiteral (drop 5 str) (currentIdx+5)
            else (currentIdx+5, drop 5 str)

        parseSubPackets0 :: String -> Int -> Int -> Int -> (Int, Int)
        parseSubPackets0 str currentIdx len versionSum
            | len == 0 = (versionSum, currentIdx)
            | otherwise =
                let (subVersion, newIdx) = parsePacket str currentIdx
                    newLen = len - (newIdx - currentIdx)
                in parseSubPackets0 str newIdx newLen (versionSum + subVersion)

        parseSubPackets1 :: String -> Int -> Int -> Int -> (Int,Int)
        parseSubPackets1 str currentIdx num versionSum
            | num == 0 = (versionSum,currentIdx)
            | otherwise =
                let (subVersion, newIdx) = parsePacket str currentIdx
                in parseSubPackets1 str newIdx (num -1) (versionSum + subVersion)
main :: IO ()
main = do
  hexStr <- readFile "input.txt"
  let binStr = hexToBin (filter (/='\n') hexStr)
  let (versionSum, _) = parsePacket binStr 0
  print versionSum
