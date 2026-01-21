import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Bits
import Data.Word
import Numeric (showHex)
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import System.IO

sList :: [Int]
sList = [7,12,17,22,7,12,17,22,7,12,17,22,7,12,17,22,
         5,9,14,20,5,9,14,20,5,9,14,20,5,9,14,20,
         4,11,16,23,4,11,16,23,4,11,16,23,4,11,16,23,
         6,10,15,21,6,10,15,21,6,10,15,21,6,10,15,21]

kList :: [Word32]
kList = map fromIntegral
  [0xd76aa478,0xe8c7b756,0x242070db,0xc1bdceee,0xf57c0faf,0x4787c62a,0xa8304613,0xfd469501,
   0x698098d8,0x8b44f7af,0xffff5bb1,0x895cd7be,0x6b901122,0xfd987193,0xa679438e,0x49b40821,
   0xf61e2562,0xc040b340,0x265e5a51,0xe9b6c7aa,0xd62f105d,0x02441453,0xd8a1e681,0xe7d3fbc8,
   0x21e1cde6,0xc33707d6,0xf4d50d87,0x455a14ed,0xa9e3e905,0xfcefa3f8,0x676f02d9,0x8d2a4c8a,
   0xfffa3942,0x8771f681,0x6d9d6122,0xfde5380c,0xa4beea44,0x4bdecfa9,0xf6bb4b60,0xbebfbc70,
   0x289b7ec6,0xeaa127fa,0xd4ef3085,0x04881d05,0xd9d4d039,0xe6db99e5,0x1fa27cf8,0xc4ac5665,
   0xf4292244,0x432aff97,0xab9423a7,0xfc93a039,0x655b59c3,0x8f0ccc92,0xffeff47d,0x85845dd1,
   0x6fa87e4f,0xfe2ce6e0,0xa3014314,0x4e0811a1,0xf7537e82,0xbd3af235,0x2ad7d2bb,0xeb86d391]

toWord32s :: B.ByteString -> [Word32]
toWord32s bs = [ le i | i <- [0..15] ]
  where
    le i = let b0 = fromIntegral (B.index bs (i*4))
               b1 = fromIntegral (B.index bs (i*4+1))
               b2 = fromIntegral (B.index bs (i*4+2))
               b3 = fromIntegral (B.index bs (i*4+3))
           in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)

rotl :: Word32 -> Int -> Word32
rotl w s = (w `shiftL` s) .|. (w `shiftR` (32 - s))

processBlock :: (Word32,Word32,Word32,Word32) -> B.ByteString -> (Word32,Word32,Word32,Word32)
processBlock (a0,b0,c0,d0) block = (a0 + a', b0 + b', c0 + c', d0 + d')
  where
    x = toWord32s block
    (a',b',c',d') = loop 0 a0 b0 c0 d0
    loop 64 a b c d = (a,b,c,d)
    loop i a b c d =
      let (f,g) | i <= 15 = ((b .&. c) .|. ((complement b) .&. d), i)
                | i <= 31 = ((d .&. b) .|. ((complement d) .&. c), (5*i + 1) `mod` 16)
                | i <= 47 = (b `xor` c `xor` d, (3*i + 5) `mod` 16)
                | otherwise = (c `xor` (b .|. (complement d)), (7*i) `mod` 16)
          temp = b + rotl (a + f + kList !! i + x !! g) (sList !! i)
      in loop (i+1) d temp b c

pad :: B.ByteString -> B.ByteString
pad bs = B.concat [bs, B.singleton 0x80, zeros, lenBytes]
  where
    origLen = fromIntegral (B.length bs) :: Word64
    padLen = (56 - ((B.length bs + 1) `mod` 64)) `mod` 64
    zeros = B.replicate padLen 0
    len = origLen * 8
    lenBytes = B.pack [fromIntegral (len `shiftR` (8*k) .&. 0xff) | k <- [0..7]]

chunks64 :: B.ByteString -> [B.ByteString]
chunks64 bs = [B.take 64 (B.drop (i*64) bs) | i <- [0..(B.length bs `div` 64 - 1)]]

word32ToBytesLE :: Word32 -> [Word8]
word32ToBytesLE w = map (\k -> fromIntegral (w `shiftR` (8*k) .&. 0xff)) [0..3]

md5 :: B.ByteString -> String
md5 bs = concatMap toHexByte bytes
  where
    initA = 0x67452301
    initB = 0xefcdab89
    initC = 0x98badcfe
    initD = 0x10325476
    padded = pad bs
    (a,b,c,d) = foldl processBlock (initA,initB,initC,initD) (chunks64 padded)
    bytes = concatMap word32ToBytesLE [a,b,c,d]
    toHexByte b = let h = showHex b "" in if length h == 1 then '0':h else h

openDoors :: String -> [Bool]
openDoors h = [c `elem` "bcdef" | c <- take 4 h]

validMove :: Int -> Int -> Char -> Bool
validMove x y 'U' = y > 0
validMove x y 'D' = y < 3
validMove x y 'L' = x > 0
validMove x y 'R' = x < 3
validMove _ _ _ = False

bfs :: String -> String
bfs pass = go (Seq.singleton (0,0,""))
  where
    dirs = "UDLR"
    dx = [0,0,-1,1]
    dy = [-1,1,0,0]
    go Seq.Empty = ""
    go ( (x,y,p) :<| q )
      | x == 3 && y == 3 = p
      | otherwise =
          let h = md5 (BC.pack (pass ++ p))
              od = openDoors h
              nexts = [ (x + dx!!i, y + dy!!i, p ++ [dirs!!i]) | i <- [0..3], od!!i, validMove x y (dirs!!i) ]
              q' = foldl (|>) q nexts
          in go q'

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  s <- hGetLine h
  hClose h
  putStrLn (bfs s)