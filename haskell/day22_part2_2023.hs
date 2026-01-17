
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (sortOn, foldl')

data Brick = Brick { bId :: Int, x1 :: Int, y1 :: Int, z1 :: Int, x2 :: Int, y2 :: Int, z2 :: Int }

split :: Char -> String -> [String]
split c s = case break (== c) s of
    (w, "") -> [w]
    (w, _:rest) -> w : split c rest

parseBrick :: Int -> String -> Brick
parseBrick i s = 
    let [l, r] = split '~' s
        [lx, ly, lz] = map read (split ',' l)
        [rx, ry, rz] = map read (split ',' r)
    in Brick i (min lx rx) (min ly ry) (min lz rz) (max lx rx) (max ly ry) (max lz rz)

intersects :: Brick -> Brick -> Bool
intersects b1 b2 = max (x1 b1) (x1 b2) <= min (x2 b1) (x2 b2) &&
                   max (y1 b1) (y1 b2) <= min (y2 b1) (y2 b2)

settle :: [Brick] -> [Brick]
settle bs = reverse $ foldl' update [] (sortOn z1 bs)
  where
    update settled b =
        let overlaps = filter (intersects b) settled
            newZ1 = if null overlaps then 1 else maximum (map (\o -> z2 o + 1) overlaps)
            h = z2 b - z1 b
        in b { z1 = newZ1, z2 = newZ1 + h } : settled

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let initialBricks = zipWith parseBrick [0..] (lines contents)
    let bricks = settle initialBricks
    let ids = map bId bricks
    
    let adj = IM.fromListWith IS.union 
                [ (bId l, IS.singleton (bId u)) 
                | u <- bricks, l <- bricks, z1 u == z2 l + 1, intersects u l ]
    
    let revAdj = IM.fromListWith IS.union 
                [ (bId u, IS.singleton (bId l)) 
                | u <- bricks, l <- bricks, z1 u == z2 l + 1, intersects u l ]

    let isSafe i = all (\v -> IS.size (IM.findWithDefault IS.empty v revAdj) > 1) 
                       (IS.toList $ IM.findWithDefault IS.empty i adj)
    
    putStrLn $ "Part 1: " ++ show (length $ filter isSafe ids)

    let countFallen start = IS.size (bfs (IS.singleton start) [start]) - 1
          where
            bfs fallen [] = fallen
            bfs fallen (u:us) =
                let children = IS.toList $ IM.findWithDefault IS.empty u adj
                    newlyFallen = filter (\v -> not (IS.member v fallen) && 
                                               IS.isSubsetOf (IM.findWithDefault IS.empty v revAdj) fallen) children
                    nextFallen = foldl' (flip IS.insert) fallen newlyFallen
                in bfs nextFallen (us ++ newlyFallen)

    putStrLn $ "Part 2: " ++ show (sum $ map countFallen ids)

