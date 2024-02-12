
import qualified Data.Set as Set

data Coordinate = Coordinate Int Int Int deriving (Eq, Ord)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let initialState = lines contents
        activeCubes = foldl (\acc (y, line) -> foldl (\acc' (x, char) -> if char == '#' then Set.insert (Coordinate x y 0) acc' else acc') acc (zip [0..] line)) Set.empty (zip [0..] initialState)
        finalState = iterate simulateCycle activeCubes !! 6
    print $ Set.size finalState

simulateCycle :: Set.Set Coordinate -> Set.Set Coordinate
simulateCycle activeCubes = newActiveCubes
  where
    newActiveCubes = Set.fromList [(Coordinate x y z) | x <- [minX-1..maxX+1], y <- [minY-1..maxY+1], z <- [minZ-1..maxZ+1], let count = countNeighbors (Coordinate x y z), count == 3 || (count == 2 && Set.member (Coordinate x y z) activeCubes)]
    countNeighbors (Coordinate x y z) = length $ filter (\coord -> Set.member coord activeCubes) [(Coordinate (x+dx) (y+dy) (z+dz)) | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], (dx, dy, dz) /= (0, 0, 0)]
    minX = minimum $ map (\(Coordinate x _ _) -> x) $ Set.toList activeCubes
    maxX = maximum $ map (\(Coordinate x _ _) -> x) $ Set.toList activeCubes
    minY = minimum $ map (\(Coordinate _ y _) -> y) $ Set.toList activeCubes
    maxY = maximum $ map (\(Coordinate _ y _) -> y) $ Set.toList activeCubes
    minZ = minimum $ map (\(Coordinate _ _ z) -> z) $ Set.toList activeCubes
    maxZ = maximum $ map (\(Coordinate _ _ z) -> z) $ Set.toList activeCubes
