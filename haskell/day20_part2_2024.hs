
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Control.Monad (guard)

type Pos = (Int, Int)

main :: IO ()
main = do
    ls <- lines <$> readFile "input.txt"
    let h = length ls
        w = length (head ls)
        grid = [ ((r,c), ch) | (r,l) <- zip [0..] ls, (c,ch) <- zip [0..] l ]
        s = head [ p | (p,'S') <- grid ]
        e = head [ p | (p,'E') <- grid ]
        walls = S.fromList [ p | (p,'#') <- grid ]
        track = S.fromList [ p | (p,ch) <- grid, ch /= '#' ]

        distFromS = bfs walls track s
        distFromE = bfs walls track e
        Just normalCost = M.lookup e distFromS

        cheats = [ (p0,p1,d0+d1+man p0 p1)
               | p0 <- S.toList track
               , Just d0 <- [M.lookup p0 distFromS]
               , p1 <- S.toList track
               , Just d1 <- [M.lookup p1 distFromE]
               , let d = man p0 p1
               , d <= 20
               , let saving = normalCost - (d0 + d1 + d)
               , saving >= 100
               ]

    print (length cheats)

bfs :: S.Set Pos -> S.Set Pos -> Pos -> M.Map Pos Int
bfs walls track start = go [(start,0)] M.empty
  where
    go [] dist = dist
    go ((p,d):q) dist
        | M.member p dist = go q dist
        | otherwise =
            let dist' = M.insert p d dist
                ns = [ (x+dx,y+dy) | (dx,dy) <- [(1,0),(-1,0),(0,1),(0,-1)]
                     , let np = (x+dx,y+dy)
                     , S.member np track
                     , not (S.member np walls)
                     , not (M.member np dist')
                     ]
                x = fst p
                y = snd p
            in go (q ++ map (,d+1) ns) dist'

man :: Pos -> Pos -> Int
man (x0,y0) (x1,y1) = abs (x0-x1) + abs (y0-y1)
