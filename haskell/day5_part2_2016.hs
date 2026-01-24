import Crypto.Hash (hash, MD5, Digest)
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import Data.Char (isDigit, digitToInt)
import Data.Array.IO
import Control.Monad (forM)

md5Hex :: ByteString -> String
md5Hex bs = show (hash bs :: Digest MD5)

solve :: String -> IO String
solve door = do
  pwd <- newArray (0,7) Nothing :: IO (IOArray Int (Maybe Char))
  let go !idx !filled =  -- strict counters
        if filled == 8 then do
          chars <- forM [0..7] $ \i -> readArray pwd i
          pure [c | Just c <- chars]
        else do
          let hex = md5Hex (B.pack (door ++ show idx))
          if take 5 hex == "00000"
            then do
              let posChar = hex !! 5
                  valChar = hex !! 6
              if isDigit posChar
                then do
                  let pos = digitToInt posChar
                  if pos < 8
                    then do
                      cur <- readArray pwd pos
                      case cur of
                        Nothing -> writeArray pwd pos (Just valChar) >> go (idx+1) (filled+1)
                        Just _  -> go (idx+1) filled
                    else go (idx+1) filled
                else go (idx+1) filled
            else go (idx+1) filled
  go 0 0

main :: IO ()
main = do
  content <- readFile "input.txt"
  let door = takeWhile (`notElem` ("\r\n" :: String)) content
  putStrLn =<< solve door
