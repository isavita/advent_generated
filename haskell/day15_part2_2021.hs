
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
import Data.Char (digitToInt)
import Data.Array.Unboxed
import Control.Monad (forM_, when)
import Data.Array.ST
import Control.Monad.ST

main :: IO ()
main = do
    content <- B.readFile "input.txt"
    let rawGrid = map (map digitToInt . B.unpack) (B.lines content)
        h = length rawGrid
        w = length (head rawGrid)
        h' = h * 5
        w' = w * 5
        grid = array ((0, 0), (h' - 1, w' - 1)) $ do
            r <- [0..h' - 1]
            c <- [0..w' - 1]
            let (rB, rR) = r `divMod` h
                (cB, cR) = c `divMod` w
                val = (rawGrid !! rR !! cR + rB + cB - 1) `mod` 9 + 1
            return ((r, c), val)
    print $ solve h' w' grid

solve :: Int -> Int -> UArray (Int, Int) Int -> Int
solve h w grid = runST $ do
    dist <- newArray ((0, 0), (h - 1, w - 1)) (maxBound :: Int) :: ST s (STUArray s (Int, Int) Int)
    writeArray dist (0, 0) 0
    let pq = S.singleton (0, 0, 0)
    dijkstra dist pq
  where
    target = (h - 1, w - 1)
    dijkstra dist pq = case S.minView pq of
        Nothing -> return (-1)
        Just ((d, r, c), pq') -> do
            dCurrent <- readArray dist (r, c)
            if d > dCurrent then dijkstra dist pq'
            else if (r, c) == target then return d
            else do
                let neighbors = [(r+1, c), (r-1, c), (r, c+1), (r, c-1)]
                newPQ <- foldM' (\acc (nr, nc) -> 
                    if nr >= 0 && nr < h && nc >= 0 && nc < w
                    then do
                        let nd = d + (grid ! (nr, nc))
                        od <- readArray dist (nr, nc)
                        if nd < od then do
                            writeArray dist (nr, nc) nd
                            return $ S.insert (nd, nr, nc) acc
                        else return acc
                    else return acc) pq' neighbors
                dijkstra dist newPQ

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' f z0 xs = foldr (\x k z -> f z x >>= k) return xs z0

