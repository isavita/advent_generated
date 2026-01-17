
import System.IO
import Data.IORef
import Data.Char (isDigit)

data BP = BP { bId, oO, cO, obO, obC, gO, gOb :: Int }

parse line = 
    let n = map read $ words $ map (\c -> if isDigit c then c else ' ') line
    in BP (n!!0) (n!!1) (n!!2) (n!!3) (n!!4) (n!!5) (n!!6)

wait need cur speed
    | cur >= need = 0
    | speed <= 0 = 1000
    | otherwise = (need - cur + speed - 1) `div` speed

solve bp duration = do
    best <- newIORef 0
    let maxO = maximum [oO bp, cO bp, obO bp, gO bp]
        go t o c ob rO rC rOb g = do
            b <- readIORef best
            if g + (t * (t - 1)) `div` 2 <= b then return ()
            else do
                if g > b then writeIORef best g else return ()
                
                let wG = max (wait (gO bp) o rO) (wait (gOb bp) ob rOb)
                if t > wG + 1 then
                    go (t - wG - 1) (o + (wG+1)*rO - gO bp) (c + (wG+1)*rC) (ob + (wG+1)*rOb - gOb bp) rO rC rOb (g + t - wG - 1)
                else return ()

                let wOb = max (wait (obO bp) o rO) (wait (obC bp) c rC)
                if rOb < gOb bp && t > wOb + 1 then
                    go (t - wOb - 1) (o + (wOb+1)*rO - obO bp) (c + (wOb+1)*rC - obC bp) (ob + (wOb+1)*rOb) rO rC (rOb + 1) g
                else return ()

                let wC = wait (cO bp) o rO
                if rC < obC bp && t > wC + 1 then
                    go (t - wC - 1) (o + (wC+1)*rO - cO bp) (c + (wC+1)*rC) (ob + (wC+1)*rOb) rO (rC + 1) rOb g
                else return ()

                let wO = wait (oO bp) o rO
                if rO < maxO && t > wO + 1 then
                    go (t - wO - 1) (o + (wO+1)*rO - oO bp) (c + (wO+1)*rC) (ob + (wO+1)*rOb) (rO + 1) rC rOb g
                else return ()
    go duration 0 0 0 1 0 0 0
    readIORef best

main = do
    s <- readFile "input.txt"
    let bps = map parse (lines s)
    res <- mapM (\b -> solve b 32) (take 3 bps)
    print (product res)

