
import qualified Data.IntSet as IS
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M
import Data.Array
import Data.Char
import Data.Bits
import Data.List (sort, foldl')

type Point = (Int, Int)

main :: IO ()
main = do
    gridStr <- readFile "input.txt"
    let gridLines = lines gridStr
        h = length gridLines
        w = length (head gridLines)
        grid = listArray ((0, 0), (h - 1, w - 1)) (concat gridLines)
        pts = assocs grid
        start = head [p | (p, '@') <- pts]
        keyChars = sort [c | (_, c) <- pts, isLower c]
        kMap = M.fromList $ zip (sort $ filter (const True) keyChars) [0..]
        numKeys = M.size kMap
        target = (1 `shiftL` numKeys) - 1
        
        getIdx c = M.lookup c kMap
        pack (y, x) m = (m `shiftL` 16) .|. (y `shiftL` 8) .|. x

        bfs Seq.Empty _ = putStrLn "-1"
        bfs ((pos@(y, x), mask, dist) Seq.:<| rest) visited
            | mask == target = print dist
            | otherwise = 
                let neighbors = [ ((ny, nx), nm)
                                | (dy, dx) <- [(0, 1), (0, -1), (1, 0), (-1, 0)]
                                , let ny = y + dy, let nx = x + dx
                                , inRange (bounds grid) (ny, nx)
                                , let c = grid ! (ny, nx)
                                , c /= '#'
                                , not (isUpper c) || maybe False (testBit mask) (getIdx (toLower c))
                                , let nm = if isLower c then maybe mask (setBit mask) (getIdx c) else mask
                                , not (IS.member (pack (ny, nx) nm) visited)
                                ]
                    newVisited = foldl' (\v (p, m) -> IS.insert (pack p m) v) visited neighbors
                    newQueue = rest Seq.>< Seq.fromList [(p, m, dist + 1) | (p, m) <- neighbors]
                in bfs newQueue newVisited

    bfs (Seq.singleton (start, 0, 0)) (IS.singleton (pack start 0))

