import           Crypto.Hash (Digest, MD5, hash)
import           Data.ByteArray.Encoding (convertToBase, Base(Base16))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import           Data.Word (Word8)
import qualified Data.Vector.Mutable as MV
import           System.Environment (getArgs)

-- md5 -> lowercase hex bytes (length 32)
md5HexBS :: BS.ByteString -> BS.ByteString
md5HexBS = convertToBase Base16 . (hash :: BS.ByteString -> Digest MD5)

intDecBS :: Int -> BS.ByteString
intDecBS n = BL.toStrict (BB.toLazyByteString (BB.intDec n))

-- 2016 extra rehashes (AoC 2016 Day 14 "key stretching")
stretchHash :: BS.ByteString -> Int -> BS.ByteString
stretchHash !salt !idx = go (0 :: Int) (md5HexBS (salt <> intDecBS idx))
  where
    go !k !h
      | k == 2016 = h
      | otherwise = go (k + 1) (md5HexBS h)

hasTriplet :: BS.ByteString -> Maybe Word8
hasTriplet bs = go 0
  where
    !len = BS.length bs
    go !i
      | i + 2 >= len = Nothing
      | otherwise =
          let a = BSU.unsafeIndex bs i
              b = BSU.unsafeIndex bs (i + 1)
              c = BSU.unsafeIndex bs (i + 2)
          in if a == b && b == c then Just a else go (i + 1)

hasQuintuplet :: Word8 -> BS.ByteString -> Bool
hasQuintuplet ch bs = go 0 0
  where
    !len = BS.length bs
    go !i !run
      | i >= len  = False
      | otherwise =
          let x = BSU.unsafeIndex bs i
          in if x == ch
               then let !run' = run + 1
                    in run' >= 5 || go (i + 1) run'
               else go (i + 1) 0

-- dynamic O(1) cache: empty ByteString means "not computed"
type Cache = MV.IOVector BS.ByteString

ensureCapacity :: Cache -> Int -> IO Cache
ensureCapacity v i
  | i < MV.length v = pure v
  | otherwise = do
      let !newLen = max (i + 1) (MV.length v * 2)
      v' <- MV.replicate newLen BS.empty
      MV.copy (MV.slice 0 (MV.length v) v') v
      pure v'

main :: IO ()
main = do
  -- salt from input.txt by default; or pass as first CLI arg
  args <- getArgs
  saltStr <-
    case args of
      (s:_) -> pure s
      _     -> do
        content <- readFile "input.txt"
        pure (takeWhile (`notElem` "\r\n") content)

  let !salt = BSC.pack saltStr

  v0 <- MV.replicate 4096 BS.empty

  let getHash :: Cache -> Int -> IO (Cache, BS.ByteString)
      getHash v i = do
        v1 <- ensureCapacity v i
        h  <- MV.read v1 i
        if BS.null h
          then do
            let !h' = stretchHash salt i
            MV.write v1 i h'
            pure (v1, h')
          else pure (v1, h)

      checkNext1000 :: Cache -> Word8 -> Int -> IO (Cache, Bool)
      checkNext1000 v ch start = go v start (start + 1000)
        where
          go !vc !i !end
            | i > end   = pure (vc, False)
            | otherwise = do
                (vc', h) <- getHash vc i
                if hasQuintuplet ch h
                  then pure (vc', True)
                  else go vc' (i + 1) end

      findKeys :: Cache -> Int -> Int -> IO Int
      findKeys v !idx !found
        | found == 64 = pure (idx - 1)
        | otherwise = do
            (v1, h) <- getHash v idx
            case hasTriplet h of
              Nothing -> findKeys v1 (idx + 1) found
              Just ch -> do
                (v2, ok) <- checkNext1000 v1 ch (idx + 1)
                if ok
                  then findKeys v2 (idx + 1) (found + 1)
                  else findKeys v2 (idx + 1) found

  result <- findKeys v0 0 0
  print result
