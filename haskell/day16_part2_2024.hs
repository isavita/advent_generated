
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    grid <- lines <$> readFile "input.txt"
    let n = length grid
        m = length (head grid)
        cells = [((r, c), grid !! r !! c) | r <- [0..n-1], c <- [0..m-1]]
        findChar ch = fst . head . filter ((== ch) . snd) $ cells
        (sx, sy) = findChar 'S'
        (ex, ey) = findChar 'E'
        dx = [-1, 0, 1, 0]
        dy = [0, 1, 0, -1]
        
        dijkstra pq dists
            | S.null pq = dists
            | distU < fromMaybe (maxBound :: Int) (M.lookup (ux, uy, ud) dists) = dijkstra pq' dists
            | otherwise = dijkstra (foldl (flip S.insert) pq'' nextStates) nextDists
          where
            ((distU, ux, uy, ud), pq') = S.deleteFindMin pq
            pq'' = pq'
            
            turns = [(distU + 1000, ux, uy, (ud + i) `mod` 4) | i <- [1, 3]]
            nx = ux + dx !! ud
            ny = uy + dy !! ud
            moves = if nx >= 0 && nx < n && ny >= 0 && ny < m && grid !! nx !! ny /= '#'
                    then [(distU + 1, nx, ny, ud)]
                    else []
            
            updates = filter (\(c, x, y, d) -> c < fromMaybe (maxBound :: Int) (M.lookup (x, y, d) dists)) (turns ++ moves)
            nextStates = updates
            nextDists = foldl (\acc (c, x, y, d) -> M.insert (x, y, d) c acc) dists updates

    let initialDist = M.singleton (sx, sy, 1) 0
        allDists = dijkstra (S.singleton (0, sx, sy, 1)) initialDist
        best = minimum [fromMaybe (maxBound :: Int) (M.lookup (ex, ey, d) allDists) | d <- [0..3]]
        
        backtrack stack visited
            | null stack = visited
            | otherwise = backtrack (nextStack ++ rest) (S.union visited (S.fromList nextStack))
          where
            (ux, uy, ud) : rest = stack
            costU = allDists M.! (ux, uy, ud)
            
            pTurns = [(ux, uy, pd) | i <- [1, 3], let pd = (ud + i) `mod` 4, 
                      M.lookup (ux, uy, pd) allDists == Just (costU - 1000)]
            px = ux - dx !! ud
            py = uy - dy !! ud
            pMove = [(px, py, ud) | px >= 0 && px < n && py >= 0 && py < m, 
                     M.lookup (px, py, ud) allDists == Just (costU - 1)]
            
            nextStack = filter (`S.notMember` visited) (pTurns ++ pMove)

    let endStates = [(ex, ey, d) | d <- [0..3], M.lookup (ex, ey, d) allDists == Just best]
        allPathStates = backtrack endStates (S.fromList endStates)
        ans = S.size $ S.map (\(x, y, _) -> (x, y)) allPathStates
    
    print ans
