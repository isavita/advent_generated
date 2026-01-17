
import System.IO
import Data.List
import qualified Data.Set as S

data BP = BP { bid, oreO, clayO, obsO, obsC, geoO, geoS :: Int }

parse :: String -> BP
parse line =
  let w = words $ map (\c -> if c >= '0' && c <= '9' then c else ' ') line
      [b, oO, cO, obO, obC, gO, gS] = map read w
  in BP b oO cO obO obC gO gS

solve :: BP -> Int -> Int
solve bp timeLimit =
  let maxO = maximum [oreO bp, clayO bp, obsO bp, geoO bp]
      maxC = obsC bp
      maxS = geoS bp
      score (ro, rc, rs, rg, o, c, s, g) = (-g, -rg, -s, -rs, -c, -rc, -o, -ro)
      step states rem =
        let nexts = S.toList $ S.fromList $ concatMap (expand bp maxO maxC maxS rem) states
        in take 1000 $ sortOn score nexts
      initial = (1, 0, 0, 0, 0, 0, 0, 0)
      finalStates = foldl' step [initial] [timeLimit, timeLimit-1 .. 1]
  in maximum $ map (\(_,_,_,_,_,_,_,g) -> g) finalStates

expand :: BP -> Int -> Int -> Int -> Int -> (Int, Int, Int, Int, Int, Int, Int, Int) -> [(Int, Int, Int, Int, Int, Int, Int, Int)]
expand bp maxO maxC maxS rem (ro, rc, rs, rg, o, c, s, g) =
  let update (nro, nrc, nrs, nrg, no, nc, ns, ng) =
        (nro, nrc, nrs, nrg, min (no+ro) ((rem-1)*maxO), min (nc+rc) ((rem-1)*maxC), min (ns+rs) ((rem-1)*maxS), ng+rg)
      opts = if o >= geoO bp && s >= geoS bp
             then [(ro, rc, rs, rg+1, o-geoO bp, c, s-geoS bp, g)]
             else [(ro, rc, rs, rg, o, c, s, g)] ++
                  [(ro, rc, rs+1, rg, o-obsO bp, c-obsC bp, s, g) | o >= obsO bp, c >= obsC bp, rs < maxS] ++
                  [(ro, rc+1, rs, rg, o-clayO bp, c, s, g) | o >= clayO bp, rc < maxC] ++
                  [(ro+1, rc, rs, rg, o-oreO bp, c, s, g) | o >= oreO bp, ro < maxO]
  in map update opts

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let bps = map parse (filter (not . null) (lines contents))
  let totalQuality = sum $ map (\bp -> bid bp * solve bp 24) bps
  print totalQuality

