
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Array

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  let h = length ls
      w = length (head ls)
      grid = listArray ((0,0),(h-1,w-1)) (concat ls)
      find c = head [(r,k) | r <- [0..h-1], k <- [0..w-1], grid!(r,k) == c]
      (sr,sc) = find 'S'
      (er,ec) = find 'E'
      dirs = [(-1,0),(0,1),(1,0),(0,-1)]
      inf = 10^15
      go pq dists
        | S.null pq = return ()
        | (r,c) == (er,ec) = print cost
        | cost > M.findWithDefault inf (r,c,d) dists = go pq' dists
        | otherwise = go (foldr S.insert pq' vs) (foldr (\(nc,nr,nk,nd) m -> M.insert (nr,nk,nd) nc m) dists vs)
        where
          ((cost,r,c,d), pq') = S.deleteFindMin pq
          ms = [(cost+1000,r,c,(d+1)`mod`4), (cost+1000,r,c,(d+3)`mod`4)] ++
               [(cost+1,nr,nk,d) | let (dr,dc)=dirs!!d, let (nr,nk)=(r+dr,c+dc), 
                inRange (bounds grid) (nr,nk), grid!(nr,nk) /= '#']
          vs = [(nc,nr,nk,nd) | (nc,nr,nk,nd) <- ms, nc < M.findWithDefault inf (nr,nk,nd) dists]
  go (S.singleton (0,sr,sc,1)) (M.singleton (sr,sc,1) 0)

