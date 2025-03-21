
import qualified Data.Map as M
import qualified Data.Set as S

data Coord = Coord {x :: Int, y :: Int} deriving (Eq, Ord, Show)

data Beam = Beam {origin :: Coord, dir :: Coord} deriving (Eq, Ord, Show)

type Grid = M.Map Coord Char

north, west, south, east :: Coord
north = Coord 0 (-1)
west = Coord (-1) 0
south = Coord 0 1
east = Coord 1 0

rotate90, rotateNeg90 :: Coord -> Coord
rotate90 (Coord x y) = Coord y (-x)
rotateNeg90 (Coord x y) = Coord (-y) x

isInBounds :: Int -> Int -> Coord -> Bool
isInBounds width height (Coord x y) = x >= 0 && x < width && y >= 0 && y < height

buildGrid :: [String] -> (Int, Int, Grid)
buildGrid inputLines = (width, height, grid)
  where
    height = length inputLines
    width = length $ head inputLines
    grid = M.fromList $ concatMap (\(y, line) -> map (\(x, char) -> (Coord x y, char)) $ filter (\(x, char) -> char /= '.') $ zip [0 ..] line) $ zip [0 ..] inputLines

nextBeam :: (Int, Int, Grid) -> Beam -> [Beam]
nextBeam (width, height, grid) beam =
  case M.lookup (origin beam) grid of
    Nothing -> [Beam (addCoord (origin beam) (dir beam)) (dir beam)]
    Just '/' ->
      let newDir = if dir beam `elem` [north, south] then rotateNeg90 (dir beam) else rotate90 (dir beam)
       in [Beam (addCoord (origin beam) newDir) newDir]
    Just '\\' ->
      let newDir = if dir beam `elem` [north, south] then rotate90 (dir beam) else rotateNeg90 (dir beam)
       in [Beam (addCoord (origin beam) newDir) newDir]
    Just '|' ->
      if dir beam `elem` [east, west]
        then
          let newDir1 = rotate90 (dir beam)
              newDir2 = rotateNeg90 (dir beam)
           in [Beam (addCoord (origin beam) newDir1) newDir1, Beam (addCoord (origin beam) newDir2) newDir2]
        else [Beam (addCoord (origin beam) (dir beam)) (dir beam)]
    Just '-' ->
      if dir beam `elem` [north, south]
        then
          let newDir1 = rotate90 (dir beam)
              newDir2 = rotateNeg90 (dir beam)
           in [Beam (addCoord (origin beam) newDir1) newDir1, Beam (addCoord (origin beam) newDir2) newDir2]
        else [Beam (addCoord (origin beam) (dir beam)) (dir beam)]
  where
    addCoord (Coord x1 y1) (Coord x2 y2) = Coord (x1 + x2) (y1 + y2)

calculatePropagation :: (Int, Int, Grid) -> Beam -> S.Set Beam
calculatePropagation (width, height, grid) start = go S.empty [start]
  where
    go seen [] = seen
    go seen (beam : beams) =
      if isInBounds width height (origin beam) && not (S.member beam seen)
        then go (S.insert beam seen) (beams ++ nextBeams)
        else go seen beams
      where
        nextBeams = nextBeam (width, height, grid) beam

calculateEnergization :: S.Set Beam -> S.Set Coord
calculateEnergization = S.map origin

solve :: [String] -> Int
solve inputLines =
  let (width, height, grid) = buildGrid inputLines
      start = Beam (Coord 0 0) east
      alreadySeen = calculatePropagation (width, height, grid) start
      alreadyEnergized = calculateEnergization alreadySeen
   in S.size alreadyEnergized

main :: IO ()
main = do
  inputLines <- lines <$> readFile "input.txt"
  print $ solve inputLines
