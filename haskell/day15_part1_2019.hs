
import qualified Data.IntMap.Strict as M
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.Int (Int64)
import Data.Sequence (ViewL(..), (|>), viewl)
import System.IO

data Computer = Computer { mem :: !(M.IntMap Int64), ip :: !Int, rb :: !Int }

run :: Computer -> [Int64] -> ([Int64], Computer)
run !c@(Computer m i r) inputs =
    let !instr = M.findWithDefault 0 i m
        !op = instr `mod` 100
        m1 = fromIntegral $ (instr `div` 100) `mod` 10
        m2 = fromIntegral $ (instr `div` 1000) `mod` 10
        m3 = fromIntegral $ (instr `div` 10000) `mod` 10
        getM' !a = M.findWithDefault 0 a m
        gp !n mode = case mode of
            0 -> getM' (fromIntegral $ getM' (i+n))
            1 -> getM' (i+n)
            2 -> getM' (fromIntegral $ getM' (i+n) + fromIntegral r)
            _ -> error ""
        sp !n mode val = M.insert (fromIntegral addr) val m
            where !addr = case mode of
                    0 -> getM' (i+n)
                    2 -> getM' (i+n) + fromIntegral r
                    _ -> error ""
    in case op of
        1 -> run (Computer (sp 3 m3 (gp 1 m1 + gp 2 m2)) (i+4) r) inputs
        2 -> run (Computer (sp 3 m3 (gp 1 m1 * gp 2 m2)) (i+4) r) inputs
        3 -> case inputs of
                (x:xs) -> run (Computer (sp 1 m1 x) (i+2) r) xs
                []     -> ([], c)
        4 -> ([gp 1 m1], Computer m (i+2) r)
        5 -> run (Computer m (if gp 1 m1 /= 0 then fromIntegral (gp 2 m2) else i+3) r) inputs
        6 -> run (Computer m (if gp 1 m1 == 0 then fromIntegral (gp 2 m2) else i+3) r) inputs
        7 -> run (Computer (sp 3 m3 (if gp 1 m1 < gp 2 m2 then 1 else 0)) (i+4) r) inputs
        8 -> run (Computer (sp 3 m3 (if gp 1 m1 == gp 2 m2 then 1 else 0)) (i+4) r) inputs
        9 -> run (Computer m (i+2) (r + fromIntegral (gp 1 m1))) inputs
        99 -> ([], c)
        _ -> error ""

type Point = (Int, Int)

move :: Point -> Int64 -> Point
move (x, y) 1 = (x, y - 1)
move (x, y) 2 = (x, y + 1)
move (x, y) 3 = (x - 1, y)
move (x, y) 4 = (x + 1, y)
move _ _ = error ""

solve :: Computer -> Int
solve start = bfs (S.singleton ((0, 0), 0, start)) (Set.singleton (0, 0))
  where
    bfs !q !v = case viewl q of
        EmptyL -> -1
        (p, d, c) :< r ->
            let (res, nq, nv) = foldl (\(f, q', v') dir ->
                    if f /= -1 then (f, q', v') else
                    let np = move p dir in
                    if Set.member np v' then (f, q', v')
                    else case run c [dir] of
                        (0:_, _) -> (f, q', Set.insert np v')
                        (2:_, _) -> (d + 1, q', Set.insert np v')
                        (1:_, nc) -> (f, q' |> (np, d + 1, nc), Set.insert np v')
                        _ -> (f, q', v')
                    ) (-1, r, v) [1..4]
            in if res /= -1 then res else bfs nq nv

main :: IO ()
main = do
    s <- readFile "input.txt"
    let p = map read $ words [if c == ',' then ' ' else c | c <- s]
        c = Computer (M.fromList $ zip [0..] p) 0 0
    let result = solve c
    if result == -1 then putStrLn "Oxygen system not found" else print result
