
import qualified Data.IntMap.Strict as M

data Computer = Computer { m :: !(M.IntMap Int), p :: !Int, r :: !Int }
data Result = Halt | Output !Int Computer | Input (Int -> Computer)

splitOn :: Char -> String -> [String]
splitOn sep s = case break (== sep) s of
    (a, []) -> [a]
    (a, _:b) -> a : splitOn sep b

step :: Computer -> Result
step (Computer mem ip rb) =
    let instr = mem M.! ip
        op = instr `mod` 100
        m1 = (instr `div` 100) `mod` 10
        m2 = (instr `div` 1000) `mod` 10
        m3 = (instr `div` 10000) `mod` 10
        get i = M.findWithDefault 0 (ip + i) mem
        val i mode = case mode of
            0 -> M.findWithDefault 0 (get i) mem
            1 -> get i
            2 -> M.findWithDefault 0 (get i + rb) mem
            _ -> 0
        tgt i mode = if mode == 0 then get i else get i + rb
    in case op of
        1  -> step $ Computer (M.insert (tgt 3 m3) (val 1 m1 + val 2 m2) mem) (ip + 4) rb
        2  -> step $ Computer (M.insert (tgt 3 m3) (val 1 m1 * val 2 m2) mem) (ip + 4) rb
        3  -> Input (\v -> Computer (M.insert (tgt 1 m1) v mem) (ip + 2) rb)
        4  -> Output (val 1 m1) (Computer mem (ip + 2) rb)
        5  -> step $ Computer mem (if val 1 m1 /= 0 then val 2 m2 else ip + 3) rb
        6  -> step $ Computer mem (if val 1 m1 == 0 then val 2 m2 else ip + 3) rb
        7  -> step $ Computer (M.insert (tgt 3 m3) (if val 1 m1 < val 2 m2 then 1 else 0) mem) (ip + 4) rb
        8  -> step $ Computer (M.insert (tgt 3 m3) (if val 1 m1 == val 2 m2 then 1 else 0) mem) (ip + 4) rb
        9  -> step $ Computer mem (ip + 2) (rb + val 1 m1)
        99 -> Halt
        _  -> Halt

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let program = map read $ splitOn ',' $ head $ lines raw
        mem = M.fromList $ zip [0..] program
        comp = Computer mem 0 0
        
        go1 c = case step c of
            Output o nc -> o : go1 nc
            _ -> []
        
        chunks [] = []
        chunks (x:y:z:xs) = (x,y,z) : chunks xs
        chunks _ = []
        
        p1 = length $ filter (\(_,_,t) -> t == 2) (chunks (go1 comp))
        
        play c px bx sc = case step c of
            Halt -> sc
            Input f -> play (f (signum (bx - px))) px bx sc
            Output x c1 -> case step c1 of
                Output y c2 -> case step c2 of
                    Output v c3 ->
                        let (nsc, npx, nbx) = if x == -1 && y == 0 then (v, px, bx)
                                              else (sc, if v == 3 then x else px, if v == 4 then x else bx)
                        in play c3 npx nbx nsc
                    _ -> sc
                _ -> sc
                
    print p1
    print $ play (comp { m = M.insert 0 2 mem }) 0 0 0

