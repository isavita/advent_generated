
import qualified Data.IntMap.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.List (foldl')

type Coord = (Int, Int)
data Machine = Machine !Int !Int !(M.IntMap Int)

runMachine :: Machine -> Int -> (Int, Machine)
runMachine (Machine ip rb mem) input = go ip rb mem where
  go i r m = let
    instr = M.findWithDefault 0 i m
    v n = let md = (instr `div` (10^(n+1))) `mod` 10; x = M.findWithDefault 0 (i+n) m
          in case md of { 0 -> M.findWithDefault 0 x m; 1 -> x; 2 -> M.findWithDefault 0 (x+r) m; _ -> 0 }
    a n = let md = (instr `div` (10^(n+1))) `mod` 10; x = M.findWithDefault 0 (i+n) m
          in case md of { 0 -> x; 2 -> x+r; _ -> 0 }
    in case instr `mod` 100 of
      1 -> go (i+4) r (M.insert (a 3) (v 1 + v 2) m)
      2 -> go (i+4) r (M.insert (a 3) (v 1 * v 2) m)
      3 -> go (i+2) r (M.insert (a 1) input m)
      4 -> (v 1, Machine (i+2) r m)
      5 -> go (if v 1 /= 0 then v 2 else i+3) r m
      6 -> go (if v 1 == 0 then v 2 else i+3) r m
      7 -> go (i+4) r (M.insert (a 3) (if v 1 < v 2 then 1 else 0) m)
      8 -> go (i+4) r (M.insert (a 3) (if v 1 == v 2 then 1 else 0) m)
      9 -> go (i+2) (r + v 1) m
      99 -> (-1, Machine i r m)
      _ -> (-1, Machine i r m)

explore :: Machine -> (Int, Coord, S.Set Coord)
explore ini = bfs (Q.singleton ((0,0), ini, 0)) (S.singleton (0,0)) (-1, (0,0), S.singleton (0,0)) where
  bfs Q.Empty _ res = res
  bfs (((x,y), mach, d) Q.:<| q) vis (p1, oc, allP) =
    let moves = [(1,(x,y+1)),(2,(x,y-1)),(3,(x-1,y)),(4,(x+1,y))]
        ns = filter (\(_,nc) -> not $ S.member nc vis) moves
        (nq, nv, np1, noc, nallP) = foldl' (\(q',v',p1',oc',ap') (dir,nc) ->
          let (out, nm) = runMachine mach dir
          in case out of
            0 -> (q', S.insert nc v', p1', oc', ap')
            1 -> (q' Q.|> (nc, nm, d+1), S.insert nc v', p1', oc', S.insert nc ap')
            2 -> (q' Q.|> (nc, nm, d+1), S.insert nc v', if p1' == -1 then d+1 else p1', nc, S.insert nc ap')
            _ -> (q', v', p1', oc', ap')) (q, vis, p1, oc, allP) ns
    in bfs nq nv (np1, noc, nallP)

maxDist :: Coord -> S.Set Coord -> Int
maxDist start passables = bfs (Q.singleton (start, 0)) (S.singleton start) 0 where
  bfs Q.Empty _ dm = dm
  bfs ((curr, d) Q.:<| q) vis dm =
    let (x,y) = curr
        ns = filter (\nc -> S.member nc passables && not (S.member nc vis)) [(x,y+1),(x,y-1),(x-1,y),(x+1,y)]
        nq = q Q.>< Q.fromList (map (\nc -> (nc, d+1)) ns)
        nv = S.union vis (S.fromList ns)
    in bfs nq nv (max dm d)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let mem = M.fromList $ zip [0..] $ map read $ words [if c == ',' then ' ' else c | c <- content]
      (p1, oc, allP) = explore (Machine 0 0 mem)
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show (maxDist oc allP)
