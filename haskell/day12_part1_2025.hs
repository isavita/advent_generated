
import Data.List (isSuffixOf, sort, nub, sortBy)
import qualified Data.IntSet as IS
import Data.Ord (comparing)

data Orientation = Orientation { pts :: [(Int, Int)], wO :: Int, hO :: Int } deriving (Eq, Ord)
data Shape = Shape { orients :: [Orientation], areaS :: Int }

trim :: String -> String
trim = let f = reverse . dropWhile (`elem` " \r\n") in f . f

generateOrients :: [(Int, Int)] -> [Orientation]
generateOrients ps = nub [ norm [(f x y, g x y) | (x, y) <- ps] | (f, g) <- trans ]
  where
    trans = [(\x y->x, \x y->y), (\x y->y, \x y-> -x), (\x y-> -x, \x y-> -y), (\x y-> -y, \x y->x)
            ,(\x y-> -x, \x y->y), (\x y->y, \x y->x), (\x y->x, \x y-> -y), (\x y-> -y, \x y-> -x)]
    norm p = let mx = minimum (map fst p); my = minimum (map snd p)
                 p' = sort [(x-mx, y-my) | (x, y) <- p]
             in Orientation p' (maximum (map fst p')+1) (maximum (map snd p')+1)

solve :: Int -> Int -> [Shape] -> IS.IntSet -> Int -> Int -> Bool
solve _ _ [] _ _ _ = True
solve w h (s:ss) grid ra fa | ra > fa = False
                            | otherwise = any tryO (orients s)
  where
    tryO (Orientation ops wo ho) = any tryP [(r, c) | r <- [0..h-ho], c <- [0..w-wo]]
      where
        tryP (r, c) = let idxs = [ (r+y)*w + (c+x) | (x, y) <- ops ]
                      in all (`IS.notMember` grid) idxs && solve w h ss (IS.union grid (IS.fromList idxs)) (ra - areaS s) (fa - areaS s)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let lp [] _ n = print n
        lp (l:ls) ss n
          | null (trim l) = lp ls ss n
          | 'x' `elem` l && ':' `elem` l =
              let (d, r) = break (==':') l
                  whStr = words [if x == 'x' then ' ' else x | x <- d]
              in if length whStr < 2 then lp ls ss n else
                  let [w, h] = map read whStr
                      qs = map read $ words (drop 1 r)
                      tf = sortBy (comparing (negate . areaS)) [sh | (sh, q) <- zip ss qs, _ <- [1..q]]
                      res = if solve w h tf IS.empty (sum $ map areaS tf) (w*h) then 1 else 0
                  in lp ls ss (n + res)
          | ":" `isSuffixOf` (trim l) =
              let (sl, rs) = span (\x -> not (null (trim x)) && not (':' `elem` x)) ls
                  pts' = [(c, r) | (r, row) <- zip [0..] sl, (c, ch) <- zip [0..] row, ch == '#']
              in if null pts' then lp rs ss n else lp rs (ss ++ [Shape (generateOrients pts') (length pts')]) n
          | otherwise = lp ls ss n
    lp (lines content) [] 0
