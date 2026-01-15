
import qualified Data.IntMap.Strict as IM
import System.IO (readFile)

type Memory = IM.IntMap Int

decode :: Int -> (Int, [Int])
decode n = (op, [m1, m2, m3])
  where
    op = n `mod` 100
    m  = n `div` 100
    m1 = m `mod` 10
    m2 = (m `div` 10) `mod` 10
    m3 = m `div` 100

readMem :: Memory -> Int -> Int
readMem mem i = IM.findWithDefault 0 i mem

writeMem :: Memory -> Int -> Int -> Memory
writeMem mem i v = IM.insert i v mem

getParam :: Memory -> Int -> Int -> Int -> Int
getParam mem rel i mode = case mode of
    0 -> readMem mem (readMem mem i)
    1 -> readMem mem i
    2 -> readMem mem (rel + readMem mem i)
    _ -> error "bad mode"

setParam :: Memory -> Int -> Int -> Int -> Int -> Memory
setParam mem rel i mode v = case mode of
    0 -> writeMem mem (readMem mem i) v
    2 -> writeMem mem (rel + readMem mem i) v
    _ -> error "bad write mode"

exec :: Memory -> Int -> Int -> [Int] -> [Int]
exec mem ip rel out = case decode (readMem mem ip) of
    (99, _) -> out
    (1, [m1,m2,m3]) ->
        let a = getParam mem rel (ip+1) m1
            b = getParam mem rel (ip+2) m2
            mem' = setParam mem rel (ip+3) m3 (a+b)
        in exec mem' (ip+4) rel out
    (2, [m1,m2,m3]) ->
        let a = getParam mem rel (ip+1) m1
            b = getParam mem rel (ip+2) m2
            mem' = setParam mem rel (ip+3) m3 (a*b)
        in exec mem' (ip+4) rel out
    (3, [m1,_,_]) ->
        let mem' = setParam mem rel (ip+1) m1 0   -- no input needed
        in exec mem' (ip+2) rel out
    (4, [m1,_,_]) ->
        let v = getParam mem rel (ip+1) m1
        in exec mem (ip+2) rel (out ++ [v])
    (5, [m1,m2,_]) ->
        let v = getParam mem rel (ip+1) m1
            j = getParam mem rel (ip+2) m2
        in exec mem (if v /= 0 then j else ip+3) rel out
    (6, [m1,m2,_]) ->
        let v = getParam mem rel (ip+1) m1
            j = getParam mem rel (ip+2) m2
        in exec mem (if v == 0 then j else ip+3) rel out
    (7, [m1,m2,m3]) ->
        let a = getParam mem rel (ip+1) m1
            b = getParam mem rel (ip+2) m2
            mem' = setParam mem rel (ip+3) m3 (if a < b then 1 else 0)
        in exec mem' (ip+4) rel out
    (8, [m1,m2,m3]) ->
        let a = getParam mem rel (ip+1) m1
            b = getParam mem rel (ip+2) m2
            mem' = setParam mem rel (ip+3) m3 (if a == b then 1 else 0)
        in exec mem' (ip+4) rel out
    (9, [m1,_,_]) ->
        let v = getParam mem rel (ip+1) m1
        in exec mem (ip+2) (rel+v) out
    _ -> error "unknown opcode"

chunks3 :: [Int] -> [(Int,Int,Int)]
chunks3 (x:y:z:rest) = (x,y,z) : chunks3 rest
chunks3 _ = []

countBlocks :: [Int] -> Int
countBlocks = length . filter (==2) . map (\(_,_,t) -> t) . chunks3

splitComma :: String -> [String]
splitComma s = case break (==',') s of
    (a, ',' : b) -> a : splitComma b
    (a, "")      -> [a]

main :: IO ()
main = do
    txt <- readFile "input.txt"
    let prog = map read $ splitComma $ filter (/='\n') txt
        mem  = IM.fromList $ zip [0..] prog
        out  = exec mem 0 0 []
    print $ countBlocks out
