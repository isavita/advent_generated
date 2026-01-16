
import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict (IntMap)
import Data.List (foldl')
import System.IO

type Grid = [Bool]          -- length 25, index 12 is ignored

parse :: String -> Grid
parse = concatMap (map (=='#')) . lines

infested :: IntMap Grid -> Int -> Int -> Bool
infested m l i = maybe False ( (!! i) ) (M.lookup l m)

neighbourCount :: IntMap Grid -> Int -> Int -> Int
neighbourCount m l i =
    let r = i `div` 5
        c = i `mod` 5
        add lvl idx acc = if infested m lvl idx then acc+1 else acc
        acc0 = acc0a where
            acc0a = foldl' (flip ($)) 0
                [ \a -> if r==0 then add (l-1) 7 a else a
                , \a -> if c==0 then add (l-1) 11 a else a
                , \a -> if c==4 then add (l-1) 13 a else a
                , \a -> if r==4 then add (l-1) 17 a else a
                , \a -> if i==7  then foldl' (\a' k -> add (l+1) k a') a [0..4] else a
                , \a -> if i==11 then foldl' (\a' k -> add (l+1) (5*k) a') a [0..4] else a
                , \a -> if i==13 then foldl' (\a' k -> add (l+1) (5*k+4) a') a [0..4] else a
                , \a -> if i==17 then foldl' (\a' k -> add (l+1) (20+k) a') a [0..4] else a
                , \a -> if r>0 && i/=17 then add l (i-5) a else a
                , \a -> if c>0 && i/=13 then add l (i-1) a else a
                , \a -> if c<4 && i/=11 then add l (i+1) a else a
                , \a -> if r<4 && i/=7  then add l (i+5) a else a
                ]
    in acc0

cellNext :: IntMap Grid -> Int -> Int -> Bool
cellNext m l i
    | i == 12 = False
    | otherwise =
        let n = neighbourCount m l i
            cur = infested m l i
        in case (cur, n) of
            (True,1) -> True
            (False,1) -> True
            (False,2) -> True
            _        -> False

minMaxLevel :: IntMap a -> (Int,Int)
minMaxLevel m = (minimum ks, maximum ks) where ks = M.keys m

clean :: IntMap Grid -> IntMap Grid
clean m
    | M.null m = m
    | otherwise =
        let (mn,mx) = minMaxLevel m
            m1 = if any id (m M.! mn) then m else M.delete mn m
            m2 = if any id (M.findWithDefault [] mx m1) then m1 else M.delete mx m1
        in m2

next2 :: IntMap Grid -> IntMap Grid
next2 m = clean $ M.fromList
    [ (l, [ cellNext m l i | i <- [0..24] ])
    | l <- [(mn-1)..(mx+1)]
    ]
  where (mn,mx) = minMaxLevel m

countBugs :: IntMap Grid -> Int
countBugs = sum . map (length . filter id) . M.elems

main :: IO ()
main = do
    content <- readFile "input.txt"
    let initGrid = parse content
        initSpace = M.singleton 0 initGrid
        finalSpace = iterate next2 initSpace !! 200
    print (countBugs finalSpace)
