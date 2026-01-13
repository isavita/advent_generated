
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import System.IO

type Memory = IM.IntMap Integer
type Grid = M.Map (Int, Int) Integer

data VM = VM { mem :: Memory, ip :: Int, rel :: Int }

loadProg :: IO Memory
loadProg = do
    txt <- readFile "input.txt"
    let vals = map read $ words $ map (\c -> if c == ',' then ' ' else c) txt
    return $ IM.fromList $ zip [0..] vals

getM :: Memory -> Int -> Integer
getM m a = IM.findWithDefault 0 a m

setM :: Memory -> Int -> Integer -> Memory
setM m a v = IM.insert a v m

mode :: Int -> Int -> Int
mode modes i = (modes `div` (10 ^ i)) `mod` 10

addr :: VM -> Int -> Int -> Int
addr VM{mem=m, rel=r} off md = case md of
    0 -> fromInteger $ getM m off
    1 -> off
    2 -> r + fromInteger (getM m off)
    _ -> error "bad mode"

val :: VM -> Int -> Int -> Integer
val vm off md = getM (mem vm) (addr vm off md)

step :: VM -> [Integer] -> (Maybe Integer, VM, [Integer])
step vm@VM{mem=m, ip=i, rel=r} inputs = case op of
    1  -> step vm{mem=setM m d (a+b), ip=i+4} inputs
    2  -> step vm{mem=setM m d (a*b), ip=i+4} inputs
    3  -> let (inp:is) = inputs
              a' = addr vm (i+1) (mode mds 0)
          in step vm{mem=setM m a' inp, ip=i+2} is
    4  -> (Just (val vm (i+1) (mode mds 0)), vm{ip=i+2}, inputs)
    5  -> step vm{ip=if a/=0 then fromInteger b else i+3} inputs
    6  -> step vm{ip=if a==0 then fromInteger b else i+3} inputs
    7  -> step vm{mem=setM m d (if a<b then 1 else 0), ip=i+4} inputs
    8  -> step vm{mem=setM m d (if a==b then 1 else 0), ip=i+4} inputs
    9  -> step vm{rel=r+fromInteger (val vm (i+1) (mode mds 0)), ip=i+2} inputs
    99 -> (Nothing, vm, inputs)
    _  -> error "unknown opcode"
  where
    instr = getM m i
    op = fromInteger $ instr `mod` 100
    mds = fromInteger $ instr `div` 100
    a = val vm (i+1) (mode mds 0)
    b = val vm (i+2) (mode mds 1)
    d = addr vm (i+3) (mode mds 2)

runUntilOutput :: VM -> [Integer] -> Maybe (Integer, VM, [Integer])
runUntilOutput vm ins = case step vm ins of
    (Nothing, _, _) -> Nothing
    (Just v, vm', ins') -> Just (v, vm', ins')
    (Nothing, _, _) -> Nothing

move :: (Int, Int) -> Int -> (Int, Int)
move (x,y) d = case d of
    0 -> (x, y+1)
    1 -> (x+1, y)
    2 -> (x, y-1)
    3 -> (x-1, y)
    _ -> (x,y)

paint :: VM -> Grid -> (Int,Int) -> Int -> Grid
paint vm grid pos dir = case runUntilOutput vm [panel] of
    Nothing -> grid
    Just (color, vm1, _) -> case runUntilOutput vm1 [] of
        Nothing -> grid
        Just (turn, vm2, _) ->
            let grid' = M.insert pos color grid
                dir'  = if turn == 0 then (dir+3) `mod` 4 else (dir+1) `mod` 4
                pos'  = move pos dir'
            in paint vm2 grid' pos' dir'
  where panel = M.findWithDefault 0 pos grid

render :: Grid -> IO ()
render grid = mapM_ row [1,0..(-5)]
  where
    row y = do
        mapM_ (cell y) [0..40]
        putStrLn ""
    cell y x = putChar $ case M.findWithDefault 0 (x,y) grid of
        0 -> ' '
        _ -> '█'

main :: IO ()
main = do
    prog <- loadProg
    let vm0 = VM {mem=prog, ip=0, rel=0}
        grid0 = M.singleton (0,0) 1
    render $ paint vm0 grid0 (0,0) 0
