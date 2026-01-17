
import Control.Monad (forM_, forM)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.Char (isDigit, isSpace)

parseLine :: String -> [((Int, Int), (Int, Int))]
parseLine s | null s' = []
            | head s' == 'x' = [((nums!!0, nums!!0), (nums!!1, nums!!2))]
            | otherwise = [((nums!!1, nums!!2), (nums!!0, nums!!0))]
  where s' = dropWhile isSpace s
        nums = parseNums s'
        parseNums "" = []
        parseNums str = let (d, r) = span (\c -> isDigit c || c == '-') (dropWhile (not . (\c -> isDigit c || c == '-')) str)
                        in if null d then [] else (read d :: Int) : parseNums r

main :: IO ()
main = do
    ls <- lines <$> readFile "input.txt"
    let clay = concatMap parseLine ls
        pts = [ (x, y) | ((x1, x2), (y1, y2)) <- clay, x <- [x1..x2], y <- [y1..y2]]
        (minX, maxX) = (minimum (map fst pts) - 1, maximum (map fst pts) + 1)
        (minY, maxY) = (minimum (map snd pts), maximum (map snd pts))
        width = maxX - minX + 1
        startX = 500 - minX
    print $ runST $ do
        grid <- newArray ((0, 0), (maxY, width - 1)) 0 :: ST s (STUArray s (Int, Int) Int)
        forM_ pts $ \(x, y) -> writeArray grid (y, x - minX) 1
        let fill x y = if y > maxY then return False else do
                v <- readArray grid (y, x)
                if v == 1 || v == 3 then return True else if v == 2 then return False else do
                    writeArray grid (y, x) 2
                    below <- fill x (y + 1)
                    if not below then return False else do
                        (lw, lx) <- spread x y (-1); (rw, rx) <- spread x y 1
                        if lw && rw then forM_ [lx..rx] (\i -> writeArray grid (y, i) 3) >> return True else return False
            spread x y dx = let nx = x + dx in if nx < 0 || nx >= width then return (False, x) else do
                vs <- readArray grid (y, nx)
                if vs == 1 then return (True, x) else do
                    writeArray grid (y, nx) 2
                    b <- fill nx (y + 1)
                    if not b then return (False, nx) else spread nx y dx
        _ <- fill startX 0
        count <- forM [minY..maxY] (\y -> forM [0..width-1] (\x -> (\v -> if v == 3 then 1 else 0) <$> readArray grid (y, x)))
        return $ sum (map sum count)
