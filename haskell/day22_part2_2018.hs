
import System.IO
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import qualified Data.Set as Set

main :: IO ()
main = do
    content <- readFile "input.txt"
    let (depth, tx, ty) = parse content
    print $ solve depth tx ty

parse :: String -> (Int, Int, Int)
parse s = 
    let ls = lines s
        d = read $ last $ words (ls !! 0)
        targetStr = last $ words (ls !! 1)
        [x, y] = map read $ splitBy ',' targetStr
    in (d, x, y)
  where
    splitBy c str = case break (==c) str of
        (w, "") -> [w]
        (w, _:rest) -> w : splitBy c rest

solve :: Int -> Int -> Int -> Int
solve depth tx ty = runST $ do
    let maxX = tx + 100
        maxY = ty + 100
        strideY = maxY + 1
        pack x y t = (x * strideY + y) * 3 + t
        
        erosion :: UArray (Int, Int) Int
        erosion = runSTUArray $ do
            arr <- newArray ((0, 0), (maxX, maxY)) 0
            forM_ [0..maxX] $ \x ->
                forM_ [0..maxY] $ \y -> do
                    v <- if (x == 0 && y == 0) || (x == tx && y == ty) 
                         then return 0
                         else if y == 0 then return (x * 16807)
                         else if x == 0 then return (y * 48271)
                         else do
                             v1 <- readArray arr (x-1, y)
                             v2 <- readArray arr (x, y-1)
                             return (v1 * v2)
                    writeArray arr (x, y) ((v + depth) `mod` 20183)
            return arr

        getType x y = erosion ! (x, y) `mod` 3
        
    distArr <- newArray (0, (maxX + 1) * strideY * 3) 1000000000 :: ST s (STUArray s Int Int)
    writeArray distArr (pack 0 0 1) 0
    
    let 
        go pq = case Set.minView pq of
            Nothing -> return (-1)
            Just ((cost, x, y, t), pq') -> do
                d <- readArray distArr (pack x y t)
                if x == tx && y == ty && t == 1 then return cost
                else if cost > d then go pq'
                else do
                    let nt = 3 - getType x y - t
                    pq1 <- update pq' (cost + 7, x, y, nt)
                    let neighbors = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
                    pq2 <- foldM (\q (nx, ny) -> 
                        if nx >= 0 && nx <= maxX && ny >= 0 && ny <= maxY && t /= getType nx ny
                        then update q (cost+1, nx, ny, t)
                        else return q) pq1 neighbors
                    go pq2

        update q (nc, nx, ny, nt) = do
            let p = pack nx ny nt
            oldD <- readArray distArr p
            if nc < oldD then do
                writeArray distArr p nc
                return $! Set.insert (nc, nx, ny, nt) q
            else return q

    go (Set.singleton (0, 0, 0, 1))

