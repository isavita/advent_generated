
import Data.List (transpose)
import qualified Data.Map.Strict as M

type Grid = [String]

rotate :: Grid -> Grid
rotate = map reverse . transpose

flipH :: Grid -> Grid
flipH = map reverse

symmetries :: Grid -> [Grid]
symmetries g = let r = take 4 (iterate rotate g) in r ++ map flipH r

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s = case break (==c) s of
    (a, _:b) -> a : splitOn c b
    (a, _)   -> [a]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

parseRule :: String -> [(Grid, Grid)]
parseRule line =
    let ws = words line
        lhs = splitOn '/' (ws !! 0)
        rhs = splitOn '/' (ws !! 2)
    in [(s, rhs) | s <- symmetries lhs]

divide :: Int -> Grid -> [[Grid]]
divide size g = map (map transpose . chunksOf size . transpose) (chunksOf size g)

combine :: [[Grid]] -> Grid
combine = concatMap (foldl1 (zipWith (++)))

step :: M.Map Grid Grid -> Grid -> Grid
step rules g =
    let n = length g
        size = if n `mod` 2 == 0 then 2 else 3
    in combine $ map (map (rules M.!)) (divide size g)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rules = M.fromList $ concatMap parseRule (lines content)
        start = [".#.", "..#", "###"]
        final = iterate (step rules) start !! 5
        ans = sum $ map (length . filter (== '#')) final
    print ans
