
import Data.List (unfoldr)
import Data.Char (isDigit)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type Memory = M.Map Int Int
data VM = VM { mem :: Memory, ip :: Int, rb :: Int, inp :: [Int], out :: [Int] }

load :: String -> IO Memory
load path = do
    s <- readFile path
    let nums = map read $ split (==',') s
    return $ M.fromList $ zip [0..] nums
  where
    split p s = case dropWhile p s of
        "" -> []
        s' -> w : split p s''
            where (w, s'') = break p s'

run :: VM -> VM
run vm = case op of
    1  -> run $ vm { mem = write 3 (par 1 + par 2), ip = ip vm + 4 }
    2  -> run $ vm { mem = write 3 (par 1 * par 2), ip = ip vm + 4 }
    3  -> run $ vm { mem = write 1 (head $ inp vm), ip = ip vm + 2, inp = tail $ inp vm }
    4  -> run $ vm { out = out vm ++ [par 1], ip = ip vm + 2 }
    5  -> run $ vm { ip = if par 1 /= 0 then par 2 else ip vm + 3 }
    6  -> run $ vm { ip = if par 1 == 0 then par 2 else ip vm + 3 }
    7  -> run $ vm { mem = write 3 (if par 1 < par 2 then 1 else 0), ip = ip vm + 4 }
    8  -> run $ vm { mem = write 3 (if par 1 == par 2 then 1 else 0), ip = ip vm + 4 }
    9  -> run $ vm { rb = rb vm + par 1, ip = ip vm + 2 }
    99 -> vm
  where
    op = (mem vm M.! ip vm) `mod` 100
    modes = [(mem vm M.! ip vm) `div` (10^i) `mod` 10 | i <- [2..4]]
    par n = case modes !! (n-1) of
        0 -> fromMaybe 0 $ M.lookup (fromMaybe 0 $ M.lookup (ip vm + n) (mem vm)) (mem vm)
        1 -> fromMaybe 0 $ M.lookup (ip vm + n) (mem vm)
        2 -> fromMaybe 0 $ M.lookup (rb vm + fromMaybe 0 (M.lookup (ip vm + n) (mem vm))) (mem vm)
    write n val = M.insert (case modes !! (n-1) of
        0 -> fromMaybe 0 $ M.lookup (ip vm + n) (mem vm)
        2 -> rb vm + fromMaybe 0 (M.lookup (ip vm + n) (mem vm)) ) val (mem vm)

beam :: Memory -> Int -> Int -> Bool
beam mem x y = head (out $ run VM { mem = mem, ip = 0, rb = 0, inp = [x,y], out = [] }) == 1

main :: IO ()
main = do
    mem <- load "input.txt"
    print $ sum [1 | y <- [0..49], x <- [0..49], beam mem x y]
