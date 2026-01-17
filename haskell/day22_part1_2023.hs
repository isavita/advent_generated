
import Data.List (sortBy, foldl')
import Data.Ord (comparing)
import qualified Data.Set as S

data B = B { ax :: Int, ay :: Int, az :: Int, bx :: Int, by :: Int, bz :: Int, idn :: Int }

parse :: Int -> String -> B
parse n s = let
  p = map read $ words [if c `elem` ",~" then ' ' else c | c <- s] :: [Int]
  in B (min (p!!0) (p!!3)) (min (p!!1) (p!!4)) (min (p!!2) (p!!5))
       (max (p!!0) (p!!3)) (max (p!!1) (p!!4)) (max (p!!2) (p!!5)) n

ov :: B -> B -> Bool
ov a b = max (ax a) (ax b) <= min (bx a) (bx b) &&
         max (ay a) (ay b) <= min (by a) (by b)

st :: [B] -> B -> [B]
st ss b = let
  os = filter (ov b) ss
  mz = if null os then 0 else maximum (map bz os)
  nz = mz + 1
  in b { az = nz, bz = nz + bz b - az b } : ss

main :: IO ()
main = do
  f <- readFile "input.txt"
  let bs = zipWith parse [0..] (lines f)
      ss = foldl' st [] (sortBy (comparing az) bs)
      sb = [ sps | b <- ss, let sps = [idn s | s <- ss, bz s + 1 == az b, ov s b], not (null sps) ]
      es = S.fromList [ head sps | sps <- sb, length sps == 1 ]
  print $ length ss - S.size es

