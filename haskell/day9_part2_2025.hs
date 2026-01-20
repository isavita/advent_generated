
import Data.List (sort, nub, foldl')
import qualified Data.Map.Strict as M
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad (forM_, filterM, when)
import Control.Monad.ST (ST)
import Data.Int (Int64)
import qualified Data.Sequence as Seq

type Pt = (Int, Int)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let pts = map ((\[x, y] -> (read x, read y)) . words . map (\c -> if c == ',' then ' ' else c)) $ filter (not . null) $ lines content
    if null pts then putStrLn "No points found." else do
        let xs = sort $ nub $ map fst pts
            ys = sort $ nub $ map snd pts
            xIdx = M.fromList $ zip xs [0..]
            yIdx = M.fromList $ zip ys [0..]
            w = 2 * length xs + 1
            h = 2 * length ys + 1
            mkW s = 1 : concat [[1, max 0 (b - a - 1)] | (a, b) <- zip s (tail s)] ++ [1, 1]
            colW = listArray (0, w-1) (map fromIntegral (take w (mkW xs))) :: UArray Int Int64
            rowH = listArray (0, h-1) (map fromIntegral (take h (mkW ys))) :: UArray Int Int64
            toGrid (x, y) = (2 * (xIdx M.! x) + 1, 2 * (yIdx M.! y) + 1)
            grid = runSTUArray $ do
                g <- newArray ((0,0), (h-1, w-1)) 0 :: ST s (STUArray s (Int, Int) Int)
                forM_ (zip pts (tail pts ++ [head pts])) $ \(p1, p2) -> do
                    let (gx1, gy1) = toGrid p1
                        (gx2, gy2) = toGrid p2
                    if gx1 == gx2 then
                        let (s, e) = if gy1 <= gy2 then (gy1, gy2) else (gy2, gy1)
                        in forM_ [s..e] $ \y -> when (rowH ! y > 0) $ writeArray g (y, gx1) 1
                    else
                        let (s, e) = if gx1 <= gx2 then (gx1, gx2) else (gx2, gx1)
                        in forM_ [s..e] $ \x -> when (colW ! x > 0) $ writeArray g (gy1, x) 1
                writeArray g (0, 0) 2
                let bfs Seq.Empty = return ()
                    bfs ((y, x) Seq.:<| q) = do
                        let ns = [(y+1, x), (y-1, x), (y, x+1), (y, x-1)]
                        nxt <- filterM (\(ny, nx) -> if ny >= 0 && ny < h && nx >= 0 && nx < w 
                                                      then (==0) <$> readArray g (ny, nx) 
                                                      else return False) ns
                        forM_ nxt $ \pos -> writeArray g pos 2
                        bfs (q Seq.>< Seq.fromList nxt)
                bfs (Seq.singleton (0, 0))
                return g
            pref = runSTUArray $ do
                p <- newArray ((0,0), (h-1, w-1)) 0 :: ST s (STUArray s (Int, Int) Int64)
                forM_ [0..h-1] $ \y -> forM_ [0..w-1] $ \x -> do
                    let cur = if grid ! (y, x) /= 2 then (colW ! x) * (rowH ! y) else 0
                    l <- if x > 0 then readArray p (y, x-1) else return 0
                    u <- if y > 0 then readArray p (y-1, x) else return 0
                    d <- if x > 0 && y > 0 then readArray p (y-1, x-1) else return 0
                    writeArray p (y, x) (cur + l + u - d)
                return p
            getSum x1 x2 y1 y2 =
                let (lx, rx, ly, ry) = (min x1 x2, max x1 x2, min y1 y2, max y1 y2)
                    v = pref ! (ry, rx)
                    v1 = if lx > 0 then v - pref ! (ry, lx-1) else v
                    v2 = if ly > 0 then v1 - pref ! (ly-1, rx) else v1
                in if lx > 0 && ly > 0 then v2 + pref ! (ly-1, lx-1) else v2
            indexedPts = zip pts [0..]
            maxA = foldl' (\acc (p1, i) -> foldl' (\acc' (p2, j) ->
                    if i > j then acc' else
                    let cur = fromIntegral (abs (fst p1 - fst p2) + 1) * fromIntegral (abs (snd p1 - snd p2) + 1)
                    in if cur > acc' then
                        let (gx1, gy1) = toGrid p1; (gx2, gy2) = toGrid p2
                        in if getSum gx1 gx2 gy1 gy2 == cur then cur else acc'
                    else acc'
                ) acc indexedPts) 0 indexedPts
        putStrLn $ "Largest valid area: " ++ show maxA
