
import Data.List (sort, foldl')
import qualified Data.IntMap.Strict as M

data Monkey = Monkey { 
    items :: [Integer], 
    op :: Integer -> Integer, 
    divB :: Integer, 
    onT :: Int, 
    onF :: Int, 
    cnt :: Integer 
}

main :: IO ()
main = do
    txt <- readFile "input.txt"
    let mks = M.fromList . zip [0..] . map parse . blocks . lines $ txt
        fin = iterate (\m -> foldl' step m [0..M.size m - 1]) mks !! 20
        ans = product . take 2 . reverse . sort . map cnt . M.elems $ fin
    print ans

blocks :: [String] -> [[String]]
blocks [] = []
blocks ls = let (m, rest) = break null ls in m : blocks (dropWhile null rest)

parse :: [String] -> Monkey
parse ls =
    let its = map (read . filter (/=',')) $ drop 2 $ words (ls!!1)
        ws = words (ls!!2)
        f = case (ws!!4, ws!!5) of
            ("*", "old") -> \x -> x * x
            ("*", n)     -> (* read n)
            ("+", n)     -> (+ read n)
            _            -> error "Unknown"
        d = read $ last $ words (ls!!3)
        t = read $ last $ words (ls!!4)
        fl = read $ last $ words (ls!!5)
    in Monkey its f d t fl 0

step :: M.IntMap Monkey -> Int -> M.IntMap Monkey
step m i =
    let mk = m M.! i
        upd acc x = 
            let v = op mk x `div` 3
                dst = if v `mod` divB mk == 0 then onT mk else onF mk
            in M.adjust (\t -> t { items = items t ++ [v] }) dst acc
        m' = foldl' upd m (items mk)
    in M.adjust (\t -> t { items = [], cnt = cnt t + toInteger (length (items mk)) }) i m'

