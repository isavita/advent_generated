
import Data.Bits
import Data.Word
import System.IO

nextSecret :: Word64 -> Word64
nextSecret s = s'
  where
    x = s * 64
    s1 = s `xor` x
    s2 = s1 .&. 0xFFFFFF
    x2 = s2 `div` 32
    s3 = s2 `xor` x2
    s4 = s3 .&. 0xFFFFFF
    x3 = s4 * 2048
    s' = (s4 `xor` x3) .&. 0xFFFFFF

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let buyers = map (read :: String -> Word64) $ filter (not . null) $ lines contents
  let total = sum $ map (\b -> iterate nextSecret b !! 2000) buyers
  print total
