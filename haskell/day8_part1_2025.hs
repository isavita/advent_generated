
{-# LANGUAGE BangPatterns #-}

import Data.List (foldl', sort, sortBy)
import qualified Data.IntMap.Strict as IM
import Data.Ord (comparing)

-- Function to split strings based on a delimiter
splitOn :: Char -> String -> [String]
splitOn c s = 
    let (before, after) = break (== c) s
    in before : case after of
                  [] -> []
                  (_:rest) -> splitOn c rest

type Point = (Integer, Integer, Integer)

-- Calculate squared distance to maintain precision and performance
distSq :: Point -> Point -> Integer
distSq (!x1, !y1, !z1) (!x2, !y2, !z2) =
    let dx = x1 - x2
        dy = y1 - y2
        dz = z1 - z2
    in dx*dx + dy*dy + dz*dz

-- DSU (Disjoint Set Union) Data Structure
data DSU = DSU { 
    parents :: !(IM.IntMap Int), 
    sizes   :: !(IM.IntMap Int) 
}

-- Initialize each node as its own parent with a size of 1
initDSU :: Int -> DSU
initDSU n = DSU 
    (IM.fromList [(i, i) | i <- [0..n-1]]) 
    (IM.fromList [(i, 1) | i <- [0..n-1]])

-- Find the root of a node with path compression
find :: Int -> IM.IntMap Int -> (Int, IM.IntMap Int)
find !i !ps =
    let !p = ps IM.! i
    in if p == i 
       then (i, ps)
       else let (root, ps') = find p ps
            in (root, IM.insert i root ps')

-- Unify two sets based on their sizes
unify :: DSU -> (Int, Int) -> DSU
unify !dsu (!i, !j) =
    let (rootI, ps') = find i (parents dsu)
        (rootJ, ps'') = find j ps'
    in if rootI == rootJ
       then dsu { parents = ps'' }
       else 
           let !szI = sizes dsu IM.! rootI
               !szJ = sizes dsu IM.! rootJ
           in if szI < szJ
              then DSU (IM.insert rootI rootJ ps'') (IM.insert rootJ (szI + szJ) (sizes dsu))
              else DSU (IM.insert rootJ rootI ps'') (IM.insert rootI (szI + szJ) (sizes dsu))

-- Parse "X,Y,Z" string into a Point tuple
parsePoint :: String -> Point
parsePoint s = 
    case splitOn ',' (filter (/= ' ') s) of
        [x, y, z] -> (read x, read y, read z)
        _         -> error "Invalid line format"

main :: IO ()
main = do
    -- Read and parse input
    content <- readFile "input.txt"
    let lines' = filter (not . null) $ lines content
        points = map parsePoint lines'
        n = length points
        indexedPoints = zip [0..] points

    -- 1. Generate all unique pairs and calculate their squared distances
    let edges = [ (distSq p1 p2, i, j) 
                | (i, p1) <- indexedPoints
                , (j, p2) <- indexedPoints
                , i < j ]

    -- 2. Select the 1000 pairs with the shortest distances
    let sortedEdges = sort edges
        top1000 = take 1000 sortedEdges

    -- 3. Apply Union-Find logic to connect the junction boxes
    let initialDSU = initDSU n
        finalDSU = foldl' (\acc (_, i, j) -> unify acc (i, j)) initialDSU top1000

    -- 4. Retrieve sizes of all circuits (roots in the DSU)
    let ps = parents finalDSU
        szs = sizes finalDSU
        allRootSizes = [ sz | (i, p) <- IM.toList ps, i == p, let sz = szs IM.! i ]
    
    -- 5. Find the three largest circuit sizes and compute their product
    let top3Sizes = take 3 $ sortBy (comparing negate) allRootSizes
    print $ product top3Sizes
