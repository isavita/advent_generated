{-# LANGUAGE OverloadedStrings #-}
import Crypto.Hash (hash, MD5, Digest)
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

md5Raw :: ByteString -> ByteString
md5Raw bs = let digest = hash bs :: Digest MD5
            in B.pack (show digest)

-- Check for 6 leading hex zeros = first 3 bytes of raw digest are 0
has6ZeroHex :: ByteString -> Bool
has6ZeroHex bs = 
  let digest = hash bs :: Digest MD5
      hex = B.pack (show digest)
  in B.take 6 hex == "000000"

findCoin :: ByteString -> Int
findCoin key = go 0
  where
    go n = let input = key <> B.pack (show n)
           in if has6ZeroHex input then n else go (n + 1)

main :: IO ()
main = do
  content <- B.readFile "input.txt"
  let key = B.takeWhile (\c -> c /= '\r' && c /= '\n') content
  print (findCoin key)
