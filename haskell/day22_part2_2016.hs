
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Char (isDigit)

data Node = Node { usedN :: Int }

parseLine :: String -> ((Int, Int), Node)
parseLine line =
    let ws = words line
        p = ws !! 0
        x = read $ takeWhile isDigit $ drop 1 $ dropWhile (/= 'x') p
        y = read $ takeWhile isDigit $ drop 1 $ dropWhile (/= 'y') p
        u = read $ init (ws !! 2)
    in ((x, y), Node u)

bfs :: (Int, Int) -> (Int, Int) -> (Int, Int) -> S.Set (Int, Int) -> (Int, Int) -> Int
bfs start target (mw, mh) walls obstacle = go (Seq.singleton (start, 0)) (S.singleton start)
  where
    go Seq.Empty _ = 1000000
    go ((curr@(cx, cy), d) Seq.:<| rest) vis
        | curr == target = d
        | otherwise =
            let ns = [(cx+1, cy), (cx-1, cy), (cx, cy+1), (cx, cy-1)]
                valid p@(nx, ny) = nx >= 0 && nx <= mw && ny >= 0 && ny <= mh 
                    && not (S.member p walls) && p /= obstacle && not (S.member p vis)
                nexts = filter valid ns
            in go (rest Seq.>< Seq.fromList [(n, d+1) | n <- nexts]) (foldr S.insert vis nexts)

solve :: (Int, Int) -> (Int, Int) -> S.Set (Int, Int) -> (Int, Int) -> Int -> Int
solve goal hole walls dims acc
    | goal == (0, 0) = acc
    | otherwise =
        let next = (fst goal - 1, snd goal)
            d = bfs hole next dims walls goal
        in solve next goal walls dims (acc + d + 1)

main :: IO ()
main = do
    c <- readFile "input.txt"
    let ls = drop 2 $ lines c
        nodes = map parseLine ls
        mw = maximum $ map (fst . fst) nodes
        mh = maximum $ map (snd . fst) nodes
        hole = fst $ head $ filter (\(_, n) -> usedN n == 0) nodes
        walls = S.fromList $ map fst $ filter (\(_, n) -> usedN n > 400) nodes
        ans = solve (mw, 0) hole walls (mw, mh) 0
    print ans
