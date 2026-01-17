
import Data.List (foldl', sort)
import qualified Data.IntMap.Strict as IM

data Monkey = Monkey [Integer] (Integer -> Integer) Integer Int Int Integer

main :: IO ()
main = do
    s <- readFile "input.txt"
    let blocks [] = []
        blocks l = case break (== "") l of (a, b) -> a : blocks (dropWhile (== "") b)
        mks_list = map (parse . unlines) . filter (not . null) . blocks $ lines s
        mks = IM.fromList $ zip [0..] mks_list
        mval = product [d | Monkey _ _ d _ _ _ <- IM.elems mks]
        res = iterate (step mval) mks !! 10000
        counts = sort [c | Monkey _ _ _ _ _ c <- IM.elems res]
    print $ product . take 2 . reverse $ counts

parse :: String -> Monkey
parse m =
    let l = lines m
        its = map (read . filter (`elem` ['0'..'9'])) . drop 2 $ words (l !! 1)
        ops = drop 4 $ words (l !! 2)
        d = read . last $ words (l !! 3)
        t = read . last $ words (l !! 4)
        f = read . last $ words (l !! 5)
        fop = case ops of
            ["*", "old"] -> \x -> x * x
            ["*", n]     -> \x -> x * read n
            ["+", "old"] -> \x -> x + x
            ["+", n]     -> \x -> x + read n
            _            -> id
    in Monkey its fop d t f 0

step :: Integer -> IM.IntMap Monkey -> IM.IntMap Monkey
step mval mmap = foldl' (turn mval) mmap [0..IM.size mmap - 1]

turn :: Integer -> IM.IntMap Monkey -> Int -> IM.IntMap Monkey
turn mval mmap i =
    let Monkey its op d t f c = mmap IM.! i
        m' = IM.insert i (Monkey [] op d t f (c + toInteger (length its))) mmap
        throw m v =
            let v' = op v `rem` mval
                dest = if v' `rem` d == 0 then t else f
            in IM.adjust (\(Monkey is o d' t' f' c') -> Monkey (is ++ [v']) o d' t' f' c') dest m
    in foldl' throw m' its
