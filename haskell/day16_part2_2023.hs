
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import Data.List (maximumBy)
import Data.Ord (comparing)

type Pos = (Int, Int)
type Dir = (Int, Int)
type State = (Pos, Dir)
type Grid = [String]

reflect :: Dir -> Char -> Dir
reflect (dx, dy) '/' = (-dy, -dx)
reflect (dx, dy) '\\' = (dy, dx)
reflect d _ = d

splitBeam :: Dir -> Char -> [Dir]
splitBeam (dx, dy) '|' | dx /= 0 = [(0, -1), (0, 1)]
splitBeam (dx, dy) '-' | dy /= 0 = [(-1, 0), (1, 0)]
splitBeam _ _ = []

simulateBeam :: Grid -> Pos -> Dir -> Set Pos
simulateBeam grid startPos startDir = go empty empty [(startPos, startDir)]
  where
    (height, width) = (length grid, length (head grid))
    inBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height

    go :: Set Pos -> Set State -> [State] -> Set Pos
    go energized visited [] = energized
    go energized visited (currentState@(pos@(x, y), dir@(dx, dy)):rest)
      | currentState `member` visited = go energized visited rest
      | otherwise =
        let newVisited = insert currentState visited
            newEnergized = insert pos energized
            nextPos = (x + dx, y + dy)
        in if not (inBounds nextPos)
           then go newEnergized newVisited rest
           else case grid !! snd nextPos !! fst nextPos of
                  '.' -> go newEnergized newVisited ((nextPos, dir):rest)
                  c | c `elem` ['/', '\\'] ->
                    let newDir = reflect dir c
                    in go newEnergized newVisited ((nextPos, newDir):rest)
                  c | c `elem` ['|', '-'] ->
                    let newDirs = splitBeam dir c
                        nextStates = map (\d -> (nextPos, d)) newDirs
                    in if null newDirs
                       then go newEnergized newVisited ((nextPos, dir) : rest)
                       else go newEnergized newVisited (nextStates ++ rest)
                  _ -> go newEnergized newVisited ((nextPos, dir):rest)

getEdgeStartPositions :: Grid -> [(Pos, Dir)]
getEdgeStartPositions grid =
    let height = length grid
        width = length (head grid)
    in [( (x,0), (0,1)) | x <- [0..width-1] ] ++
       [( (x,height-1), (0,-1)) | x <- [0..width-1] ] ++
       [( (0,y), (1,0)) | y <- [1..height-2] ] ++
       [( (width-1,y), (-1,0)) | y <- [1..height-2] ]

partTwo :: Grid -> Int
partTwo grid =
  let starts = getEdgeStartPositions grid
      energizedCounts = map (\(pos, dir) -> Set.size $ simulateBeam grid pos dir) starts
  in maximum energizedCounts

main :: IO ()
main = do
  grid <- lines <$> readFile "input.txt"
  let part1 = Set.size $ simulateBeam grid (0, 0) (1, 0)
  print part1
  let part2 = partTwo grid
  print part2
