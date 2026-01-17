
import qualified Data.IntMap as M

data Rule = L Char | A [[Int]]

main :: IO ()
main = do
    f <- readFile "input.txt"
    let (rl, ml') = break null (lines f)
        ml = if null ml' then [] else tail ml'
        rs = M.fromList $ map p rl
        p s = let (i, r) = break (==':') s 
                  content = drop 2 r
              in (read i, if head content == '"' then L (content !! 1) else A (map (map read . words) (splt '|' content)))
        splt c s = let (a, b) = break (==c) s in a : if null b then [] else splt c (tail b)
        match i s = case rs M.! i of
            L c -> if not (null s) && head s == c then [tail s] else []
            A as -> concatMap (ms s) as
        ms s [] = [s]
        ms s (r:rx) = concatMap (`ms` rx) (match r s)
    print $ length $ filter (any null . match 0) ml
