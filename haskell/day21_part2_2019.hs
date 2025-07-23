
import qualified Data.Map as M
import Data.Char (ord)
import Data.List (intercalate)

type Memory = M.Map Int Int
data VM = VM { mem :: Memory, ip :: Int, inp :: [Int], out :: [Int], rb :: Int }

load :: String -> IO Memory
load path = do
    s <- readFile path
    let nums = map read (words [if c==',' then ' ' else c | c <- s]) :: [Int]
    return $ M.fromList (zip [0..] nums)

run :: VM -> VM
run vm@VM{mem=m,ip=i,inp=in_,out=o,rb=r} =
    let cmd = M.findWithDefault 0 i m
        op  = cmd `mod` 100
        modes = [(cmd `div` (10^(p+1))) `mod` 10 | p <- [1..3]]
        param n = let v = M.findWithDefault 0 (i+n) m
                      mode = modes !! (n-1)
                  in case mode of
                       0 -> M.findWithDefault 0 v m
                       1 -> v
                       2 -> M.findWithDefault 0 (r+v) m
        addr n = let v = M.findWithDefault 0 (i+n) m
                 in if modes !! (n-1) == 2 then r+v else v
    in case op of
         1  -> run vm{mem=M.insert (addr 3) (param 1 + param 2) m, ip=i+4}
         2  -> run vm{mem=M.insert (addr 3) (param 1 * param 2) m, ip=i+4}
         3  -> case in_ of
                 (x:xs) -> run vm{mem=M.insert (addr 1) x m, ip=i+2, inp=xs}
                 []     -> vm
         4  -> run vm{out=o ++ [param 1], ip=i+2}
         5  -> run vm{ip=if param 1 /= 0 then param 2 else i+3}
         6  -> run vm{ip=if param 1 == 0 then param 2 else i+3}
         7  -> run vm{mem=M.insert (addr 3) (if param 1 < param 2 then 1 else 0) m, ip=i+4}
         8  -> run vm{mem=M.insert (addr 3) (if param 1 == param 2 then 1 else 0) m, ip=i+4}
         9  -> run vm{rb=r + param 1, ip=i+2}
         99 -> vm
         _  -> vm

sendString :: String -> [Int]
sendString = (++[10]) . map ord

main :: IO ()
main = do
    mem <- load "input.txt"
    let cmds = ["NOT A J","NOT B T","OR T J","NOT C T","OR T J","AND D J",
                "NOT A T","AND A T","OR E T","OR H T","AND T J","RUN"]
        input = concatMap sendString cmds
        vm = run VM{mem=mem,ip=0,inp=input,out=[],rb=0}
    mapM_ print (out vm)
