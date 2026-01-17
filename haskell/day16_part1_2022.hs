
import qualified Data.Map.Strict as M
import Data.List (foldl', delete)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = map parseLine (lines input)
        ids = [v | (v, _, _) <- parsed]
        flows = M.fromList [(v, f) | (v, f, _) <- parsed]
        initialDists = M.fromList $ [((v, v), 0) | v <- ids] ++ [((v, t), 1) | (v, _, ts) <- parsed, t <- ts]
        dists = foldl' (\dk k -> foldl' (\di i -> foldl' (\dj j ->
            let dij = M.findWithDefault 1000 (i, j) dj
                dik = M.findWithDefault 1000 (i, k) dj
                dkj = M.findWithDefault 1000 (k, j) dj
            in if dik + dkj < dij then M.insert (i, j) (dik + dkj) dj else dj) di ids) dk ids) initialDists ids
        openValves = [v | (v, f, _) <- parsed, f > 0]
        solve curr time total open =
            foldl' (\m next ->
                let d = dists M.! (curr, next)
                    ntime = time - d - 1
                in if ntime > 0
                   then max m (solve next ntime (total + ntime * (flows M.! next)) (delete next open))
                   else m
            ) total open
    print $ solve "AA" 30 0 openValves

parseLine :: String -> (String, Int, [String])
parseLine line =
    let w = words $ map (\c -> if c `elem` "=;," then ' ' else c) line
    in (w !! 1, read (w !! 5), drop 10 w)

