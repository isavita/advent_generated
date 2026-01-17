
import qualified Data.Set as S
import Data.Char (digitToInt)
import Data.Array
import Data.List (foldl')

main :: IO ()
main = do
    input <- readFile "input.txt"
    let ls = lines input
        grid = listArray ((0, 0), (length ls - 1, length (head ls) - 1)) [digitToInt c | l <- ls, c <- l]
        bnds = bounds grid
        target = snd bnds
        solve pq visited
            | S.null pq = -1
            | (r, c) == target && n >= 4 = cost
            | S.member state visited = solve pq' visited
            | otherwise = solve (foldl' (flip S.insert) pq' neighbors) (S.insert state visited)
          where
            ((cost, r, c, dr, dc, n), pq') = S.deleteFindMin pq
            state = (r, c, dr, dc, n)
            neighbors = [ (cost + grid ! (nr, nc), nr, nc, ndr, ndc, nn)
                        | (ndr, ndc) <- [(0, 1), (0, -1), (1, 0), (-1, 0)]
                        , (ndr, ndc) /= (-dr, -dc)
                        , let (nr, nc) = (r + ndr, c + ndc)
                        , inRange bnds (nr, nc)
                        , let isSame = (ndr == dr && ndc == dc)
                              nn = if isSame then n + 1 else 1
                        , if dr == 0 && dc == 0 then True 
                          else if isSame then n < 10 else n >= 4
                        ]
    print $ solve (S.singleton (0, 0, 0, 0, 0, 0)) S.empty

