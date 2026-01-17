
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import Data.Char (chr)
import System.IO

type Mem = IM.IntMap Int
type Pos = (Int,Int)

decode :: Int -> (Int,[Int])
decode n = (op,[m1,m2,m3])
  where op = n `mod` 100
        m1 = (n `div` 100) `mod` 10
        m2 = (n `div` 1000) `mod` 10
        m3 = (n `div` 10000) `mod` 10

getVal :: Mem -> Int -> Int -> Int -> Int
getVal mem rel i mode = case mode of
  0 -> IM.findWithDefault 0 (IM.findWithDefault 0 i mem) mem
  1 -> IM.findWithDefault 0 i mem
  2 -> IM.findWithDefault 0 (rel + IM.findWithDefault 0 i mem) mem
  _ -> 0

setVal :: Mem -> Int -> Int -> Int -> Int -> Mem
setVal mem rel i mode v = case mode of
  0 -> IM.insert (IM.findWithDefault 0 i mem) v mem
  2 -> IM.insert (rel + IM.findWithDefault 0 i mem) v mem
  _ -> mem

run :: Mem -> [Int] -> [Int]
run prog input = go prog 0 0 input []
  where
    go mem ip rel ins out = case decode (IM.findWithDefault 0 ip mem) of
      (99,_) -> reverse out
      (op,modes) ->
        let p n = getVal mem rel (ip+n) (modes!!(n-1))
            w n v = setVal mem rel (ip+n) (modes!!(n-1)) v
        in case op of
          1 -> go (w 3 (p 1 + p 2)) (ip+4) rel ins out
          2 -> go (w 3 (p 1 * p 2)) (ip+4) rel ins out
          3 -> case ins of
                 []     -> go mem ip rel ins out
                 (i:is) -> go (w 1 i) (ip+2) rel is out
          4 -> go mem (ip+2) rel ins (p 1:out)
          5 -> go mem (if p 1 /= 0 then p 2 else ip+3) rel ins out
          6 -> go mem (if p 1 == 0 then p 2 else ip+3) rel ins out
          7 -> go (w 3 (if p 1 < p 2 then 1 else 0)) (ip+4) rel ins out
          8 -> go (w 3 (if p 1 == p 2 then 1 else 0)) (ip+4) rel ins out
          9 -> go mem (ip+2) (rel + p 1) ins out
          _ -> go mem (ip+1) rel ins out

parseGrid :: [Int] -> (S.Set Pos, Pos, Int)
parseGrid out = go out 0 0 S.empty (0,0) 0
  where
    go [] _ _ sc r d = (sc,r,d)
    go (o:os) x y sc r d =
      let c = chr o in
      if c == '\n' then go os 0 (y+1) sc r d
      else
        let (sc',r',d') = case c of
              '^' -> (S.insert (x,y) sc,(x,y),0)
              '>' -> (S.insert (x,y) sc,(x,y),1)
              'v' -> (S.insert (x,y) sc,(x,y),2)
              '<' -> (S.insert (x,y) sc,(x,y),3)
              '#' -> (S.insert (x,y) sc,r,d)
              _   -> (sc,r,d)
        in go os (x+1) y sc' r' d'

alignSum :: S.Set Pos -> Int
alignSum sc = sum [x*y | (x,y) <- S.toList sc, all (`S.member` sc) [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]]

main :: IO ()
main = do
  txt <- readFile "input.txt"
  let prog = IM.fromList $ zip [0..] (map read $ words $ map (\c -> if c==',' then ' ' else c) txt)
      out = run prog []
      (grid,_,_) = parseGrid out
  print $ alignSum grid
