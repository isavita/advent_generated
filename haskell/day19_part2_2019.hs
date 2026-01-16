
import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict (IntMap)
import System.IO

type Mem = IntMap Int

readMem :: FilePath -> IO Mem
readMem f = do
    s <- readFile f
    let xs = map read $ split ',' $ filter (/= '\n') s
    return $ M.fromList $ zip [0..] xs
  where
    split _ [] = []
    split d cs = let (h,t) = break (==d) cs
                 in h : case t of [] -> []; (_:rest) -> split d rest

get :: Mem -> Int -> Int
get m i = M.findWithDefault 0 i m

set :: Mem -> Int -> Int -> Mem
set m i v = M.insert i v m

run :: Mem -> [Int] -> Int
run m0 inp0 = go 0 0 m0 inp0 []
  where
    go ip rel m inp out = case op of
        1  -> go (ip+4) rel (set m (wa 3) (p 1 + p 2)) inp out
        2  -> go (ip+4) rel (set m (wa 3) (p 1 * p 2)) inp out
        3  -> case inp of
                i:is -> go (ip+2) rel (set m (wa 1) i) is out
                []   -> error "no input"
        4  -> go (ip+2) rel m inp (p 1 : out)
        5  -> go (if p 1 /= 0 then p 2 else ip+3) rel m inp out
        6  -> go (if p 1 == 0 then p 2 else ip+3) rel m inp out
        7  -> go (ip+4) rel (set m (wa 3) (if p 1 < p 2 then 1 else 0)) inp out
        8  -> go (ip+4) rel (set m (wa 3) (if p 1 == p 2 then 1 else 0)) inp out
        9  -> go (ip+2) (rel + p 1) m inp out
        99 -> case out of v:_ -> v; [] -> error "no output"
        _  -> error "bad opcode"
      where
        instr = get m ip
        op = instr `mod` 100
        mode n = (instr `div` (10^(n+1))) `mod` 10
        p n = let a = get m (ip+n)
               in case mode n of
                    0 -> get m a
                    1 -> a
                    2 -> get m (rel + a)
                    _ -> error "bad mode"
        wa n = let a = get m (ip+n)
                in case mode n of
                     0 -> a
                     2 -> rel + a
                     _ -> error "bad write mode"

beam :: Mem -> Int -> Int -> Bool
beam prog x y = run prog [x,y] == 1

search :: Mem -> Int -> Int -> Int
search prog x y
    | not (beam prog x y)          = search prog (x+1) y
    | not (beam prog (x+99) y)     = search prog x (y+1)
    | not (beam prog x (y+99))     = search prog (x+1) y
    | otherwise                    = x*10000 + y

main :: IO ()
main = do
    prog <- readMem "input.txt"
    print $ search prog 0 20
