{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Bits
import Data.Word
import System.IO
import Data.Char (isSpace)
import Numeric (showIntAtBase)
import Data.List (unfoldr)

k :: [Word32]
k = map fromIntegral
  [0xd76aa478,0xe8c7b756,0x242070db,0xc1bdceee,0xf57c0faf,0x4787c62a,0xa8304613,0xfd469501
  ,0x698098d8,0x8b44f7af,0xffff5bb1,0x895cd7be,0x6b901122,0xfd987193,0xa679438e,0x49b40821
  ,0xf61e2562,0xc040b340,0x265e5a51,0xe9b6c7aa,0xd62f105d,0x02441453,0xd8a1e681,0xe7d3fbc8
  ,0x21e1cde6,0xc33707d6,0xf4d50d87,0x455a14ed,0xa9e3e905,0xfcefa3f8,0x676f02d9,0x8d2a4c8a
  ,0xfffa3942,0x8771f681,0x6d9d6122,0xfde5380c,0xa4beea44,0x4bdecfa9,0xf6bb4b60,0xbebfbc70
  ,0x289b7ec6,0xeaa127fa,0xd4ef3085,0x04881d05,0xd9d4d039,0xe6db99e5,0x1fa27cf8,0xc4ac5665
  ,0xf4292244,0x432aff97,0xab9423a7,0xfc93a039,0x655b59c3,0x8f0ccc92,0xffeff47d,0x85845dd1
  ,0x6fa87e4f,0xfe2ce6e0,0xa3014314,0x4e0811a1,0xf7537e82,0xbd3af235,0x2ad7d2bb,0xeb86d391]

r :: [Int]
r =
  [7,12,17,22,7,12,17,22,7,12,17,22,7,12,17,22
  ,5,9,14,20,5,9,14,20,5,9,14,20,5,9,14,20
  ,4,11,16,23,4,11,16,23,4,11,16,23,4,11,16,23
  ,6,10,15,21,6,10,15,21,6,10,15,21,6,10,15,21]

toWord32LE :: B.ByteString -> Int -> Word32
toWord32LE bs off =
  let b0 = fromIntegral (B.index bs off) :: Word32
      b1 = fromIntegral (B.index bs (off+1)) :: Word32
      b2 = fromIntegral (B.index bs (off+2)) :: Word32
      b3 = fromIntegral (B.index bs (off+3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)

pad :: B.ByteString -> B.ByteString
pad msg =
  let len = B.length msg
      bitLen :: Word64
      bitLen = fromIntegral len * 8
      padLen = ((56 - ((len + 1) `mod` 64)) `mod` 64) + 1
      zeros = B.replicate (padLen - 1) 0
      lenBytes = B.pack $ map fromIntegral
        [ fromIntegral (bitLen .&. 0xff)
        , fromIntegral ((bitLen `shiftR` 8) .&. 0xff)
        , fromIntegral ((bitLen `shiftR` 16) .&. 0xff)
        , fromIntegral ((bitLen `shiftR` 24) .&. 0xff)
        , fromIntegral ((bitLen `shiftR` 32) .&. 0xff)
        , fromIntegral ((bitLen `shiftR` 40) .&. 0xff)
        , fromIntegral ((bitLen `shiftR` 48) .&. 0xff)
        , fromIntegral ((bitLen `shiftR` 56) .&. 0xff)
        ]
  in B.concat [msg, B.singleton 0x80, zeros, lenBytes]

processBlock :: (Word32,Word32,Word32,Word32) -> B.ByteString -> (Word32,Word32,Word32,Word32)
processBlock (h0,h1,h2,h3) blk =
  let w i = toWord32LE blk (i*4)
      loop i a b c d
        | i==64 = (a,b,c,d)
        | otherwise =
          let (f,g) = if i <= 15 then ((b .&. c) .|. (complement b .&. d), i)
                      else if i <= 31 then ((d .&. b) .|. (complement d .&. c), (5*i + 1) `mod` 16)
                      else if i <= 47 then (b `xor` c `xor` d, (3*i + 5) `mod` 16)
                      else (c `xor` (b .|. complement d), (7*i) `mod` 16)
              temp = d
              x = a + f + (k !! i) + (w g)
              newb = b + rotateL x (r !! i)
          in loop (i+1) temp newb b c
      (a',b',c',d') = loop 0 h0 h1 h2 h3
  in (h0 + a', h1 + b', h2 + c', h3 + d')

md5 :: B.ByteString -> (Word32,Word32,Word32,Word32)
md5 msg =
  let m = pad msg
      chunks = unfoldr (\b -> if B.null b then Nothing else Just (B.splitAt 64 b)) m
      (a,b,c,d) = foldl processBlock (0x67452301,0xEFCDAB89,0x98BADCFE,0x10325476) chunks
  in (a,b,c,d)

trimEnd :: B.ByteString -> B.ByteString
trimEnd = C.dropWhileEnd isSpace

loopSearch :: B.ByteString -> Integer -> IO ()
loopSearch key n = do
  let s = C.pack (show n)
      bs = B.append key s
      (a,_,_,_) = md5 bs
  if (a .&. 0x00F0FFFF) == 0
    then print n
    else loopSearch key (n+1)

main :: IO ()
main = do
  inp <- B.readFile "input.txt"
  let key = trimEnd inp
  loopSearch key 0