import Crypto.Hash (hash, MD5, Digest)
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)

md5Hex :: ByteString -> String
md5Hex bs = show (hash bs :: Digest MD5)

-- Find password: collect 6th hex char when hash starts with "00000"
solve :: String -> String
solve door = go 0 []
  where
    go _ acc | length acc == 8 = reverse acc
    go idx acc =
      let input = B.pack (door ++ show idx)
          hex = md5Hex input
      in if take 5 hex == "00000"
         then go (idx + 1) (hex !! 5 : acc)
         else go (idx + 1) acc

main :: IO ()
main = do
  content <- readFile "input.txt"
  let door = takeWhile (`notElem` "\r\n") content
  putStrLn (solve door)
