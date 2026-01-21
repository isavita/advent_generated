import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Bits
import Data.Word
import Numeric (showHex)
import Data.List (isInfixOf)
import System.IO

md5 :: B.ByteString -> String
md5 msg = concatMap wordToHexLE finalState
  where
    len = B.length msg
    bitlen :: Word64
    bitlen = fromIntegral len * 8
    padLen = fromIntegral $ (56 - ((len + 1) `mod` 64)) `mod` 64
    padding = B.pack (0x80 : replicate padLen 0)
    lenBytes = B.pack [fromIntegral ((bitlen `shiftR` (8*i)) .&. 0xff) | i <- [0..7]]
    msg' = B.concat [msg, padding, lenBytes]
    chunks = chunk64 msg'
    initialState = [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476] :: [Word32]
    finalState = foldl md5Transform initialState chunks

chunk64 :: B.ByteString -> [B.ByteString]
chunk64 bs
  | B.null bs = []
  | otherwise = let (h,t) = B.splitAt 64 bs in h : chunk64 t

word32FromLE :: B.ByteString -> Int -> Word32
word32FromLE b off = foldr (\(i,shiftN) acc -> acc .|. (fromIntegral (B.index b (off + i)) `shiftL` shiftN)) 0 [(0,0),(1,8),(2,16),(3,24)]

md5Transform :: [Word32] -> B.ByteString -> [Word32]
md5Transform [aa,bb,cc,dd] block =
  let x = [word32FromLE block (i*4) | i <- [0..15]]
      a0=aa; b0=bb; c0=cc; d0=dd
      f x y z = (x .&. y) .|. (complement x .&. z)
      g x y z = (x .&. z) .|. (y .&. complement z)
      h x y z = x `xor` y `xor` z
      i_ x y z = y `xor` (x .|. complement z)
      rotl v s = rotateL v s
      ff a b c d xk s t = (rotl (a + f b c d + xk + t) s) + b
      gg a b c d xk s t = (rotl (a + g b c d + xk + t) s) + b
      hh a b c d xk s t = (rotl (a + h b c d + xk + t) s) + b
      ii a b c d xk s t = (rotl (a + i_ b c d + xk + t) s) + b
      -- Round 1
      a1 = ff a0 b0 c0 d0 (x!!0) 7  0xd76aa478
      d1 = ff d0 a1 b0 c0 (x!!1) 12 0xe8c7b756
      c1 = ff c0 d1 a1 b0 (x!!2) 17 0x242070db
      b1 = ff b0 c1 d1 a1 (x!!3) 22 0xc1bdceee
      a2 = ff a1 b1 c1 d1 (x!!4) 7  0xf57c0faf
      d2 = ff d1 a2 b1 c1 (x!!5) 12 0x4787c62a
      c2 = ff c1 d2 a2 b1 (x!!6) 17 0xa8304613
      b2 = ff b1 c2 d2 a2 (x!!7) 22 0xfd469501
      a3 = ff a2 b2 c2 d2 (x!!8) 7  0x698098d8
      d3 = ff d2 a3 b2 c2 (x!!9) 12 0x8b44f7af
      c3 = ff c2 d3 a3 b2 (x!!10) 17 0xffff5bb1
      b3 = ff b2 c3 d3 a3 (x!!11) 22 0x895cd7be
      a4 = ff a3 b3 c3 d3 (x!!12) 7  0x6b901122
      d4 = ff d3 a4 b3 c3 (x!!13) 12 0xfd987193
      c4 = ff c3 d4 a4 b3 (x!!14) 17 0xa679438e
      b4 = ff b3 c4 d4 a4 (x!!15) 22 0x49b40821
      -- Round 2
      a5 = gg a4 b4 c4 d4 (x!!1) 5  0xf61e2562
      d5 = gg d4 a5 b4 c4 (x!!6) 9  0xc040b340
      c5 = gg c4 d5 a5 b4 (x!!11) 14 0x265e5a51
      b5 = gg b4 c5 d5 a5 (x!!0) 20 0xe9b6c7aa
      a6 = gg a5 b5 c5 d5 (x!!5) 5  0xd62f105d
      d6 = gg d5 a6 b5 c5 (x!!10) 9 0x02441453
      c6 = gg c5 d6 a6 b5 (x!!15) 14 0xd8a1e681
      b6 = gg b5 c6 d6 a6 (x!!4) 20 0xe7d3fbc8
      a7 = gg a6 b6 c6 d6 (x!!9) 5  0x21e1cde6
      d7 = gg d6 a7 b6 c6 (x!!14) 9 0xc33707d6
      c7 = gg c6 d7 a7 b6 (x!!3) 14 0xf4d50d87
      b7 = gg b6 c7 d7 a7 (x!!8) 20 0x455a14ed
      a8 = gg a7 b7 c7 d7 (x!!13) 5 0xa9e3e905
      d8 = gg d7 a8 b7 c7 (x!!2) 9 0xfcefa3f8
      c8 = gg c7 d8 a8 b7 (x!!7) 14 0x676f02d9
      b8 = gg b7 c8 d8 a8 (x!!12) 20 0x8d2a4c8a
      -- Round 3
      a9  = hh a8 b8 c8 d8 (x!!5) 4  0xfffa3942
      d9  = hh d8 a9 b8 c8 (x!!8) 11 0x8771f681
      c9  = hh c8 d9 a9 b8 (x!!11) 16 0x6d9d6122
      b9  = hh b8 c9 d9 a9 (x!!14) 23 0xfde5380c
      a10 = hh a9 b9 c9 d9 (x!!1) 4  0xa4beea44
      d10 = hh d9 a10 b9 c9 (x!!4) 11 0x4bdecfa9
      c10 = hh c9 d10 a10 b9 (x!!7) 16 0xf6bb4b60
      b10 = hh b9 c10 d10 a10 (x!!10) 23 0xbebfbc70
      a11 = hh a10 b10 c10 d10 (x!!13) 4 0x289b7ec6
      d11 = hh d10 a11 b10 c10 (x!!0) 11 0xeaa127fa
      c11 = hh c10 d11 a11 b10 (x!!3) 16 0xd4ef3085
      b11 = hh b10 c11 d11 a11 (x!!6) 23 0x04881d05
      a12 = hh a11 b11 c11 d11 (x!!9) 4 0xd9d4d039
      d12 = hh d11 a12 b11 c11 (x!!12) 11 0xe6db99e5
      c12 = hh c11 d12 a12 b11 (x!!15) 16 0x1fa27cf8
      b12 = hh b11 c12 d12 a12 (x!!2) 23 0xc4ac5665
      -- Round 4
      a13 = ii a12 b12 c12 d12 (x!!0) 6  0xf4292244
      d13 = ii d12 a13 b12 c12 (x!!7) 10 0x432aff97
      c13 = ii c12 d13 a13 b12 (x!!14) 15 0xab9423a7
      b13 = ii b12 c13 d13 a13 (x!!5) 21 0xfc93a039
      a14 = ii a13 b13 c13 d13 (x!!12) 6 0x655b59c3
      d14 = ii d13 a14 b13 c13 (x!!3) 10 0x8f0ccc92
      c14 = ii c13 d14 a14 b13 (x!!10) 15 0xffeff47d
      b14 = ii b13 c14 d14 a14 (x!!1) 21 0x85845dd1
      a15 = ii a14 b14 c14 d14 (x!!8) 6 0x6fa87e4f
      d15 = ii d14 a15 b14 c14 (x!!15) 10 0xfe2ce6e0
      c15 = ii c14 d15 a15 b14 (x!!6) 15 0xa3014314
      b15 = ii b14 c15 d15 a15 (x!!13) 21 0x4e0811a1
      a16 = ii a15 b15 c15 d15 (x!!4) 6 0xf7537e82
      d16 = ii d15 a16 b15 c15 (x!!11) 10 0xbd3af235
      c16 = ii c15 d16 a16 b15 (x!!2) 15 0x2ad7d2bb
      b16 = ii b15 c16 d16 a16 (x!!9) 21 0xeb86d391
      aFinal = a0 + a16
      bFinal = b0 + b16
      cFinal = c0 + c16
      dFinal = d0 + d16
  in [aFinal, bFinal, cFinal, dFinal]

wordToHexLE :: Word32 -> String
wordToHexLE w = concatMap hex2 bytes
  where
    bytes = [fromIntegral ((w `shiftR` (8*i)) .&. 0xff) :: Word8 | i <- [0..3]]
    hex2 b = let s = showHex b "" in if length s == 1 then '0':s else s

findTriplet :: String -> Maybe Char
findTriplet (a:b:c:rest)
  | a == b && b == c = Just a
  | otherwise = findTriplet (b:c:rest)
findTriplet _ = Nothing

hasQuintuple :: Char -> String -> Bool
hasQuintuple c s = replicate 5 c `isInfixOf` s

solve :: String -> Int
solve salt = go 0 0 hashes
  where
    hashes = map (\i -> md5 (BC.pack (salt ++ show i))) [0..]
    go idx keys (h:hs)
      | keys == 64 = idx - 1
      | otherwise =
          case findTriplet h of
            Just c ->
              if any (hasQuintuple c) (take 1000 hs)
                then go (idx+1) (keys+1) hs
                else go (idx+1) keys hs
            Nothing -> go (idx+1) keys hs
    go _ _ _ = error "unexpected"

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let salt = case lines contents of
               (l:_) -> l
               [] -> ""
  print $ solve salt