
import Data.List
import qualified Data.Map as M

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rules = M.fromList [ (v, d) | l <- lines content, let w = words l, v <- variants (splitS (w !! 0)), let d = splitS (w !! 2) ]
        start = [".#.", "..#", "###"]
        result = iterate (step rules) start !! 18
    print . length . filter (== '#') $ concat result

splitS :: String -> [String]
splitS s = case break (== '/') s of
    (a, "") -> [a]
    (a, _:b) -> a : splitS b

variants :: [String] -> [[String]]
variants g = take 4 (iterate rotate g) ++ take 4 (iterate rotate (map reverse g))

rotate :: [[a]] -> [[a]]
rotate = transpose . reverse

step :: M.Map [String] [String] -> [String] -> [String]
step rs g = joinGrid $ map (map (rs M.!)) (splitGrid (if even (length g) then 2 else 3) g)

splitGrid :: Int -> [String] -> [[[String]]]
splitGrid n g = map (transpose . map (chunksOf n)) (chunksOf n g)

joinGrid :: [[[String]]] -> [String]
joinGrid = concatMap (map concat . transpose)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
