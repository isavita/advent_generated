
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Char (isLower, isUpper, toLower)
import Data.Bits (testBit, (.|.), (.&.), shiftL)
import Data.List (foldl', sort)
import qualified Data.Sequence as Seq
import Data.Array

type Pos = (Int, Int)
type Grid = Array Pos Char

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let ls = lines raw
        (h, w) = (length ls, length (head ls))
        grid0 = listArray ((0,0), (h-1, w-1)) (concat ls)
        (grid, starts) = transform grid0 h w
        keyLocs = [(c, p) | (p, c) <- assocs grid, isLower c]
        numKeys = length keyLocs
        targetMask = (1 `shiftL` numKeys) - 1
        keyToBit = M.fromList $ zip (sort $ map fst keyLocs) [0..]
        nodes = M.union (M.fromList keyLocs) (M.fromList $ zip "1234" starts)
        graph = M.mapWithKey (\_ p -> bfs grid keyToBit p) nodes
    print $ dijkstra graph targetMask keyToBit

transform :: Grid -> Int -> Int -> (Grid, [Pos])
transform g h w =
    let (ay, ax) = head [ (y,x) | y <- [1..h-2], x <- [1..w-2], g!(y,x) == '@',
                          g!(y-1,x) == '.', g!(y+1,x) == '.', g!(y,x-1) == '.', g!(y,x+1) == '.' ]
        updates = [ ((ay,ax), '#'), ((ay-1,ax), '#'), ((ay+1,ax), '#'), ((ay,ax-1), '#'), ((ay,ax+1), '#'),
                    ((ay-1,ax-1), '1'), ((ay-1,ax+1), '2'), ((ay+1,ax-1), '3'), ((ay+1,ax+1), '4') ]
    in (g // updates, [(ay-1,ax-1), (ay-1,ax+1), (ay+1,ax-1), (ay+1,ax+1)])

bfs :: Grid -> M.Map Char Int -> Pos -> M.Map Char (Int, Int)
bfs grid keyToBit startPos =
    let loop Seq.Empty _ res = res
        loop ((currP, currD, currM) Seq.:<| rest) vis res =
            let neighbors = [(y+dy, x+dx) | let (y,x) = currP, (dy,dx) <- [(-1,0),(1,0),(0,-1),(0,1)],
                             let n = (y+dy, x+dx), inRange (bounds grid) n, grid!n /= '#', not (S.member n vis)]
                process (q, v, r) n =
                    let c = grid!n
                        dm = if isUpper c then maybe 0 (shiftL 1) (M.lookup (toLower c) keyToBit) else 0
                        km = if isLower c then shiftL 1 (keyToBit M.! c) else 0
                        (nr, nm) = (if isLower c then M.insert c (currD+1, currM) r else r, currM .|. dm .|. km)
                    in (q Seq.:|> (n, currD+1, nm), S.insert n v, nr)
                (nextQ, nextVis, nextRes) = foldl' process (rest, vis, res) neighbors
            in loop nextQ nextVis nextRes
    in loop (Seq.singleton (startPos, 0, 0)) (S.singleton startPos) M.empty

dijkstra :: M.Map Char (M.Map Char (Int, Int)) -> Int -> M.Map Char Int -> Int
dijkstra graph targetMask keyToBit =
    let solve pq dMap
            | S.null pq = -1
            | m == targetMask = d
            | d > M.findWithDefault 2000000000 (ps, m) dMap = solve (S.deleteMin pq) dMap
            | otherwise = let (pq', dMap') = foldl' update (rest, dMap) transitions in solve pq' dMap'
            where
                ((d, ps, m), rest) = (S.findMin pq, S.deleteMin pq)
                transitions = [ (d + d2, nps, m .|. (1 `shiftL` (keyToBit M.! k)))
                              | (i, p) <- zip [0..] ps, (k, (d2, req)) <- M.toList (graph M.! p),
                                not (testBit m (keyToBit M.! k)), (req .&. m) == req,
                                let nps = take i ps ++ [k] ++ drop (i+1) ps ]
                update (q, dm) (nd, nps, nm) = if nd < M.findWithDefault 2000000000 (nps, nm) dm
                                               then (S.insert (nd, nps, nm) q, M.insert (nps, nm) nd dm)
                                               else (q, dm)
    in solve (S.singleton (0, "1234", 0)) (M.singleton ("1234", 0) 0)

