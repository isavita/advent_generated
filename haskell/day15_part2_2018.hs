
{-# LANGUAGE BangPatterns #-}
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sortBy, foldl', minimumBy)
import Data.Ord (comparing)

type Point = (Int, Int)
data Unit = Unit { ut :: !Char, hp :: !Int, ap :: !Int } deriving (Show, Eq)
type Grid = S.Set Point
type Units = M.Map Point Unit

neighbors :: Point -> [Point]
neighbors (y, x) = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

bfs :: Grid -> Units -> Point -> M.Map Point Int
bfs walls units start = go (S.singleton start) (S.singleton start) 0 M.empty
  where
    go frontier visited dist res
      | S.null frontier = res
      | otherwise =
          let nextRes = M.union res (M.fromSet (const dist) frontier)
              nextFrontier = S.fromList [ n | p <- S.toList frontier, n <- neighbors p
                                        , not (S.member n walls)
                                        , not (M.member n units)
                                        , not (S.member n visited) ]
          in go nextFrontier (S.union visited nextFrontier) (dist + 1) nextRes

findMove :: Grid -> Units -> Point -> Unit -> Maybe Point
findMove walls units p u =
  let targets = M.filter (\u' -> ut u' /= ut u) units
      inRange = S.fromList [ n | ep <- M.keys targets, n <- neighbors ep, not (S.member n walls), not (M.member n units) ]
      distsFromUnit = bfs walls units p
      reachableInRange = [ (d, tp) | tp <- S.toList inRange, Just d <- [M.lookup tp distsFromUnit] ]
  in if null reachableInRange then Nothing
     else let minDist = minimum (map fst reachableInRange)
              bestTarget = minimum [ tp | (d, tp) <- reachableInRange, d == minDist ]
              distsFromTarget = bfs walls (M.delete p units) bestTarget
              stepDists = [ (d, n) | n <- neighbors p, Just d <- [M.lookup n distsFromTarget] ]
          in if null stepDists then Nothing else Just $ snd $ minimum stepDists

moveIfNecessary :: Grid -> Units -> Point -> Unit -> (Point, Units)
moveIfNecessary walls units p u =
  let enemies = M.filter (\u' -> ut u' /= ut u) units
      isAdj = any (\n -> M.member n enemies) (neighbors p)
  in if isAdj then (p, units)
     else case findMove walls units p u of
            Nothing -> (p, units)
            Just nextP -> (nextP, M.insert nextP u (M.delete p units))

attackIfPossible :: Units -> Point -> Unit -> (Units, Bool)
attackIfPossible units p u =
  let adjEnemies = [ (ep, eu) | ep <- neighbors p, Just eu <- [M.lookup ep units], ut eu /= ut u ]
  in if null adjEnemies then (units, False)
     else let (ep, eu) = minimumBy (comparing (\(pos, unit) -> (hp unit, pos))) adjEnemies
              newEU = eu { hp = hp eu - ap u }
              elfDied = ut eu == 'E' && hp newEU <= 0
              newUnits = if hp newEU <= 0 then M.delete ep units else M.insert ep newEU units
          in (newUnits, elfDied)

playRound :: Grid -> Units -> (Units, Bool, Bool)
playRound walls units = foldl' unitTurn (units, False, False) (sortBy (comparing id) (M.keys units))
  where
    unitTurn (!currUnits, !finished, !elfDied) p =
      if finished then (currUnits, True, elfDied)
      else case M.lookup p currUnits of
        Nothing -> (currUnits, finished, elfDied)
        Just u ->
          let targets = M.filter (\u' -> ut u' /= ut u) currUnits in
          if M.null targets then (currUnits, True, elfDied)
          else let (p', units') = moveIfNecessary walls currUnits p u
                   (units'', elfDied') = attackIfPossible units' p' u
               in (units'', False, elfDied || elfDied')

gameLoop :: Int -> Grid -> Units -> Int
gameLoop !rounds walls units =
  let (newUnits, finished, _) = playRound walls units
  in if finished then rounds * sum (map hp (M.elems newUnits))
     else gameLoop (rounds + 1) walls newUnits

gameLoopPart2 :: Grid -> Units -> Maybe Int
gameLoopPart2 walls units = go 0 units
  where
    go !rounds currUnits =
      let (newUnits, finished, elfDied) = playRound walls currUnits
      in if elfDied then Nothing
         else if finished then Just (rounds * sum (map hp (M.elems newUnits)))
         else go (rounds + 1) newUnits

main :: IO ()
main = do
  input <- readFile "input.txt"
  let gridLines = lines input
      grid = [ ((y, x), c) | (y, row) <- zip [0..] gridLines, (x, c) <- zip [0..] row ]
      walls = S.fromList [ p | (p, '#') <- grid ]
      units3 = M.fromList [ (p, Unit c 200 3) | (p, c) <- grid, c == 'G' || c == 'E' ]
  putStrLn $ "Part 1: " ++ show (gameLoop 0 walls units3)
  let solvePart2 apElf = gameLoopPart2 walls (M.fromList [ (p, Unit c 200 (if c == 'E' then apElf else 3)) | (p, c) <- grid, c == 'G' || c == 'E' ])
  let results = [ res | ap <- [4..], Just res <- [solvePart2 ap] ]
  putStrLn $ "Part 2: " ++ show (head results)
