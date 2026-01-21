import Data.Char
import Data.Bits
import Data.List
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.MArray
import qualified Data.IntSet as S

knotHash :: String -> String
knotHash input = let lens = map ord input ++ [17,31,73,47,23]
                     allLens = concat (replicate 64 lens)
                     bitsFor n = [if testBit n i then '1' else '0' | i <- [7,6..0]]
                 in concatMap bitsFor $ runST $ do
    arr <- newListArray (0,255) [0..255] :: ST s (STUArray s Int Int)
    (_,_) <- foldM (\(pos,skip) len -> do
        let half = len `div` 2
        forM_ [0..half-1] $ \i -> do
            let a = (pos + i) `mod` 256
                b = (pos + len - 1 - i) `mod` 256
            va <- readArray arr a
            vb <- readArray arr b
            writeArray arr a vb
            writeArray arr b va
        let pos' = (pos + len + skip) `mod` 256
        return (pos', skip + 1)
        ) (0,0) allLens
    lst <- getElems arr
    return [foldl1 xor (take 16 (drop (i*16) lst)) | i <- [0..15]]

buildSet :: String -> S.IntSet
buildSet key = S.fromList [r*128 + c | r <- [0..127], (c,ch) <- zip [0..127] (knotHash (key ++ "-" ++ show r)), ch == '1']

removeRegion :: Int -> S.IntSet -> S.IntSet
removeRegion start s0 = go (S.singleton start) s0
  where
    go stack s | S.null stack = s
               | otherwise =
        let (x,stk') = S.deleteFindMin stack
            s' = if S.member x s then S.delete x s else s
            r = x `div` 128
            c = x `mod` 128
            neigh = [r*128 + c' | c' <- [c-1,c+1], c'>=0, c'<128] ++
                    [r'*128 + c | r' <- [r-1,r+1], r'>=0, r'<128]
            toAdd = filter (`S.member` s') neigh
            stack'' = foldr S.insert stk' toAdd
        in go stack'' s'

countRegions :: S.IntSet -> Int
countRegions s = go s 0
  where
    go set acc | S.null set = acc
               | otherwise =
        let x = S.findMin set
            set' = removeRegion x set
        in go set' (acc+1)

main :: IO ()
main = do
    key <- fmap (filter (/= '\r') . takeWhile (/= '\n')) $ readFile "input.txt"
    let s = buildSet key
    print (S.size s)
    print (countRegions s)