
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.List
import Data.Ord
import Data.Maybe

type Pos = (Int, Int)
data Unit = Unit { uType :: Char, uHP :: Int, uPos :: Pos, uPwr :: Int }
data Cave = Cave { walls :: S.Set Pos, units :: M.Map Pos Unit }

neighbors (y, x) = [(y-1, x), (y, x-1), (y, x+1), (y+1, x)]

bfs walls units start = go (Seq.singleton (start, 0)) (M.singleton start 0)
  where
    go Seq.Empty visited = visited
    go ((p, d) Seq.:<| q) visited =
      let ns = filter (\n -> S.notMember n walls && M.notMember n units && M.notMember n visited) (neighbors p)
          newVisited = foldl' (\m n -> M.insert n (d+1) m) visited ns
          newQ = q Seq.>< Seq.fromList [(n, d+1) | n <- ns]
      in go newQ newVisited

moveUnit u cave =
  let p = uPos u
      allUnits = units cave
      enemies = M.filter (\e -> uType e /= uType u) allUnits
      isAdj = any (\ep -> ep `elem` neighbors p) (M.keys enemies)
      inRange = S.fromList [ n | ep <- M.keys enemies, n <- neighbors ep, S.notMember n (walls cave), M.notMember n allUnits ]
  in if isAdj then (p, cave)
     else let dists = bfs (walls cave) allUnits p
              reachable = M.filterWithKey (\pos _ -> S.member pos inRange) dists
          in if M.null reachable then (p, cave)
             else let minDist = minimum (M.elems reachable)
                      target = fst . head . sort $ [ (pos, d) | (pos, d) <- M.toList reachable, d == minDist ]
                      distsBack = bfs (walls cave) allUnits target
                      possibleSteps = filter (\n -> S.notMember n (walls cave) && M.notMember n allUnits) (neighbors p)
                      stepDists = M.filterWithKey (\pos _ -> pos `elem` possibleSteps) distsBack
                  in if M.null stepDists then (p, cave)
                     else let minStepDist = minimum (M.elems stepDists)
                              step = fst . head . sort $ [ (n, d) | (n, d) <- M.toList stepDists, d == minStepDist ]
                              newU = u { uPos = step }
                          in (step, cave { units = M.insert step newU (M.delete p allUnits) })

attackUnit p cave =
  let u = (units cave) M.! p
      enemies = M.filter (\e -> uType e /= uType u) (units cave)
      adj = [ (pos, e) | pos <- neighbors p, Just e <- [M.lookup pos enemies] ]
  in if null adj then cave
     else let (ePos, _) = head $ sortBy (\(p1, e1) (p2, e2) -> compare (uHP e1) (uHP e2) <> compare p1 p2) adj
              enemy = (units cave) M.! ePos
              newHP = uHP enemy - uPwr u
          in cave { units = if newHP <= 0 then M.delete ePos (units cave) else M.insert ePos enemy{uHP = newHP} (units cave) }

roundLoop cave r =
  let order = sort (M.keys (units cave))
      go [] c = (True, c)
      go (p:ps) c =
        case M.lookup p (units c) of
          Nothing -> go ps c
          Just u -> if M.null (M.filter (\e -> uType e /= uType u) (units c)) then (False, c)
                    else let (p', c') = moveUnit u c in go ps (attackUnit p' c')
      (finished, cave') = go order cave
  in if finished then roundLoop cave' (r + 1)
     else r * sum (map uHP (M.elems (units cave')))

main = do
  content <- readFile "input.txt"
  let rows = lines content
      u = [ Unit c 200 (y, x) 3 | (y, row) <- zip [0..] rows, (x, c) <- zip [0..] row, c `elem` "EG" ]
      w = [ (y, x) | (y, row) <- zip [0..] rows, (x, c) <- zip [0..] row, c == '#' ]
      cave = Cave (S.fromList w) (M.fromList [ (uPos unit, unit) | unit <- u ])
  print $ roundLoop cave 0

