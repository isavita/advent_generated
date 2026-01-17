
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Bits (bit, (.|.), (.&.))

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let parsed = M.fromList $ map parse (lines contents)
        nodes = M.keys parsed
        initialDists = M.fromList $ [((u, v), 1) | (u, (_, ts)) <- M.toList parsed, v <- ts] ++ [((u, u), 0) | u <- nodes]
        dists = foldl' (\d k -> foldl' (\d' i -> foldl' (\d'' j ->
                  let ik = M.findWithDefault 9999 (i, k) d'
                      kj = M.findWithDefault 9999 (k, j) d'
                      ij = M.findWithDefault 9999 (i, j) d'
                  in if ik + kj < ij then M.insert (i, j) (ik + kj) d'' else d''
                ) d' nodes) d nodes) initialDists nodes
        useful = [(v, f) | (v, (f, _)) <- M.toList parsed, f > 0]
        productive = zipWith (\(v, f) i -> (v, f, bit i)) useful [0..]
        dfs t u m p acc = foldl' (\a (v, f, bi) ->
            let d = dists M.! (u, v)
                nt = t - d - 1
            in if nt > 0 && (m .&. bi) == 0
               then dfs nt v (m .|. bi) (p + nt * f) a
               else a) (IM.insertWith max m p acc) productive
        results = IM.toList $ dfs 26 "AA" 0 0 IM.empty
    print $ maximum $ 0 : [v1 + v2 | (m1, v1) <- results, (m2, v2) <- results, m1 < m2, m1 .&. m2 == 0]

parse :: String -> (String, (Int, [String]))
parse line = (ws !! 1, (read . filter (`elem` ['0'..'9']) $ ws !! 4, map (filter (/= ',')) $ drop 9 ws))
    where ws = words line

