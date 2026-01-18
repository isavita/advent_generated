
import Data.Bits
import Data.Word
import Data.Char (ord, intToDigit)
import System.IO (readFile)

ks :: [Word32]
ks = [fromIntegral $ floor (2^32 * abs (sin (fromIntegral i))) | i <- [1..64]]

ss :: [Int]
ss = [7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
      5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20,
      4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
      6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21]

md5 :: String -> String
md5 s = concatMap toHex [a, b, c, d]
  where
    toHex x = map (\i -> intToDigit $ fromIntegral (x `shiftR` i) .&. 0xF) [4, 0, 12, 8, 20, 16, 28, 24]
    (a, b, c, d) = foldl processBlock (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476) chunks
    msg = map (fromIntegral . ord) s
    len = length msg
    padded = msg ++ [0x80] ++ replicate ((56 - (len + 1) `mod` 64) `mod` 64) 0 ++ 
             map (\i -> fromIntegral ((fromIntegral len * 8 :: Word64) `shiftR` i)) [0,8..56]
    chunks = split 64 padded
    split _ [] = []
    split n l = take n l : split n (drop n l)
    processBlock (a1, b1, c1, d1) block =
        let x = [foldl1 (.|.) [fromIntegral (block !! (i*4+j)) `shiftL` (j*8) | j <- [0..3]] | i <- [0..15]]
            f i a b c d = let (func, idx) = if i < 16 then (\x y z -> (x .&. y) .|. (complement x .&. z), i)
                                           else if i < 32 then (\x y z -> (x .&. z) .|. (y .&. complement z), (5*i+1) `mod` 16)
                                           else if i < 48 then (\x y z -> x `xor` y `xor` z, (3*i+5) `mod` 16)
                                           else (\x y z -> y `xor` (x .|. complement z), (7*i) `mod` 16)
                          in b + rotateL (a + func b c d + (x !! idx) + (ks !! i)) (ss !! i)
            go [a, b, c, d] i = [d, f i a b c d, b, c]
            [a', b', c', d'] = foldl go [a1, b1, c1, d1] [0..63]
        in (a1 + a', b1 + b', c1 + c', d1 + d')

dfs :: String -> Int -> Int -> String -> Int
dfs passcode x y path
    | x == 3 && y == 3 = length path
    | otherwise =
        let h = md5 (passcode ++ path)
            dirs = [ (nx, ny, c, i) | (i, (c, dx, dy)) <- zip [0..3] [('U',0,-1), ('D',0,1), ('L',-1,0), ('R',1,0)]
                                 , let nx = x + dx, let ny = y + dy
                                 , nx >= 0 && nx < 4 && ny >= 0 && ny < 4 ]
            open = [ (nx, ny, c) | (nx, ny, c, i) <- dirs, h !! i `elem` "bcdef" ]
            results = [ dfs passcode nx ny (path ++ [c]) | (nx, ny, c) <- open ]
        in if null results then 0 else maximum results

main :: IO ()
main = do
    content <- readFile "input.txt"
    let passcode = filter (`notElem` "\n\r ") content
    if null passcode then return () else print $ dfs passcode 0 0 ""
