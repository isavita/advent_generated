
import Data.Char (isSpace)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import System.IO

type Color = String
type Entry = (Color, Int)

limit :: Color -> Int
limit "red"   = 12
limit "green" = 13
limit "blue"  = 14
limit _       = maxBound

parseLine :: String -> (Int, [Entry])
parseLine s =
    let (g, rest) = break (== ':') s
        gid = read $ drop 5 g
        clean = map (\c -> if c == ',' || c == ';' then ' ' else c) (drop 2 rest)
        ws = words clean
        entries = [(c, read n) | (n:c:_) <- chunksOf 2 ws]
    in (gid, entries)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h,t) = splitAt n xs in h : chunksOf n t

possible :: [Entry] -> Bool
possible = all (\(c,n) -> n <= limit c)

power :: [Entry] -> Int
power es =
    let m = foldl' (\acc (c,n) -> M.insertWith max c n acc) M.empty es
        r = M.findWithDefault 0 "red"   m
        g = M.findWithDefault 0 "green" m
        b = M.findWithDefault 0 "blue"  m
    in r * g * b

main :: IO ()
main = do
    content <- readFile "input.txt"
    let games = map parseLine (lines content)
        part1 = sum [i | (i,es) <- games, possible es]
        part2 = sum [power es | (_,es) <- games]
    putStrLn $ show part1 ++ " " ++ show part2
