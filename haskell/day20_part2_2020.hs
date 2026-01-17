
import Data.List (transpose, splitAt)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Tile = Tile { tid :: Integer, grid :: [[Char]] } deriving (Show, Eq)

rotate :: [[Char]] -> [[Char]]
rotate = transpose . reverse

allSymmetries :: [[Char]] -> [[[Char]]]
allSymmetries g = 
    let rots = take 4 (iterate rotate g)
    in rots ++ map reverse rots

tileOrs :: Tile -> [Tile]
tileOrs (Tile i g) = [Tile i g' | g' <- allSymmetries g]

topT, bottomT, leftT, rightT :: Tile -> [Char]
topT = head . grid
bottomT = last . grid
leftT = map head . grid
rightT = map last . grid

solve :: Int -> Int -> Int -> S.Set Integer -> M.Map (Int, Int) Tile -> [Tile] -> Maybe (M.Map (Int, Int) Tile)
solve size x y used placed tiles
    | y == size = Just placed
    | otherwise =
        let (nx, ny) = if x == size - 1 then (0, y + 1) else (x + 1, y)
            available = filter (\t -> not (tid t `S.member` used)) tiles
            possible = [ t' | t <- available, t' <- tileOrs t, fits x y t' placed ]
        in listToMaybe $ mapMaybe (\t' -> solve size nx ny (S.insert (tid t') used) (M.insert (x, y) t' placed) tiles) possible

fits :: Int -> Int -> Tile -> M.Map (Int, Int) Tile -> Bool
fits x y t placed =
    let f1 = case M.lookup (x-1, y) placed of
                Nothing -> True
                Just l  -> rightT l == leftT t
        f2 = case M.lookup (x, y-1) placed of
                Nothing -> True
                Just a  -> bottomT a == topT t
    in f1 && f2

monsterPattern :: [(Int, Int)]
monsterPattern = [ (0,18), (1,0), (1,5), (1,6), (1,11), (1,12), (1,17), (1,18), (1,19), (2,1), (2,4), (2,7), (2,10), (2,13), (2,16) ]

isMonster :: Int -> Int -> [[Char]] -> Bool
isMonster r c img = all (\(dr, dc) -> (img !! (r+dr)) !! (c+dc) == '#') monsterPattern

countMonsters :: [[Char]] -> Int
countMonsters img =
    let h = length img
        w = length (head img)
    in length [ () | r <- [0..h-3], c <- [0..w-20], isMonster r c img ]

parse :: [String] -> [Tile]
parse [] = []
parse ls = parseTile (take 11 ls) : parse (drop 12 ls)

parseTile :: [String] -> Tile
parseTile (h:g) = 
    let i = read (takeWhile (\c -> c >= '0' && c <= '9') (drop 5 h))
    in Tile i g

main :: IO ()
main = do
    content <- readFile "input.txt"
    let tiles = parse (lines content)
        size = round $ sqrt $ fromIntegral $ length tiles
        solution = fromJust $ solve size 0 0 S.empty M.empty tiles
        c1 = tid $ solution M.! (0, 0)
        c2 = tid $ solution M.! (size-1, 0)
        c3 = tid $ solution M.! (0, size-1)
        c4 = tid $ solution M.! (size-1, size-1)
        part1 = c1 * c2 * c3 * c4
        tileSize = length (grid (solution M.! (0,0))) - 2
        trimmed = M.map (map (init . tail) . init . tail . grid) solution
        image = [ concat [ (trimmed M.! (x, y)) !! r | x <- [0..size-1] ] 
                | y <- [0..size-1], r <- [0..tileSize-1] ]
        totalHashes = length $ filter (=='#') (concat image)
        monsterCount = maximum [ countMonsters o | o <- allSymmetries image ]
        part2 = totalHashes - monsterCount * 15
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
