
import Data.Char (isDigit)

data P = I Int | L [P] deriving (Eq)

instance Ord P where
    compare (I a) (I b) = compare a b
    compare (L a) (L b) = compare a b
    compare (I a) b     = compare (L [I a]) b
    compare a     (I b) = compare a     (L [I b])

p :: String -> (P, String)
p ('[':s) = let (xs, r) = pL s in (L xs, r)
  where 
    pL (']':rs) = ([], rs)
    pL rs = let (x, r1) = p rs
                (xs, r2) = case r1 of (',':ss) -> pL ss
                                      (']':ss) -> ([], ss)
                                      _        -> ([], r1)
            in (x:xs, r2)
p s = let (n, r) = span isDigit s in (I (read n), r)

main :: IO ()
main = do
    ls <- filter (not . null) . lines <$> readFile "input.txt"
    let chunk (a:b:xs) = (fst (p a), fst (p b)) : chunk xs
        chunk _        = []
        pairs          = chunk ls
        ans = sum [i | (i, (a, b)) <- zip [1..] pairs, a < b]
    putStrLn $ "Sum of indices of pairs in the right order: " ++ show ans
