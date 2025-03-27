
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (foldl', sort)
import Data.Maybe (fromMaybe)

data Coord = Coord { x :: !Int, y :: !Int } deriving (Eq, Ord, Show)
type Rocks = Map.Map Coord Char
data Grid = Grid { width :: !Int, height :: !Int, rocks :: !Rocks } deriving (Eq, Show)

-- Key for cycle detection: positions of 'O' rocks
type GridKey = Set.Set Coord

parseInput :: [String] -> Grid
parseInput inputLines =
    let h = length inputLines
        w = if h > 0 then length (head inputLines) else 0
        parseLine y line = [(Coord x y, c) | (x, c) <- zip [0..] line, c /= '.']
        allRocks = Map.fromList $ concat [parseLine y line | (y, line) <- zip [0..] inputLines]
    in Grid w h allRocks

shift :: Grid -> (Int, Int) -> Grid
shift grid@(Grid w h rks) (dx, dy) =
    let isO coord = Map.lookup coord rks == Just 'O'
        isCube coord = Map.lookup coord rks == Just '#'
        coords = Map.keys rks

        -- Determine iteration order based on direction
        orderedCoords = case (dx, dy) of
            (0, -1) -> sort coords -- North: Sort by y then x
            (-1, 0) -> sort [Coord x y | Coord y x <- sort [Coord y x | Coord x y <- coords]] -- West: Sort by x then y
            (0, 1)  -> reverse $ sort coords -- South: Sort reverse by y then x
            (1, 0)  -> reverse $ sort [Coord x y | Coord y x <- sort [Coord y x | Coord x y <- coords]] -- East: Sort reverse by x then y
            _       -> error "Invalid direction"

        moveRock :: Rocks -> Coord -> Rocks
        moveRock currentRocks coord
            | Map.lookup coord currentRocks /= Just 'O' = currentRocks -- Only move 'O' rocks
            | otherwise =
                let currentPos = coord
                    nextPos = Coord (x currentPos + dx) (y currentPos + dy)

                    tryMove :: Coord -> Coord -> Rocks
                    tryMove !curr !next
                        | nx < 0 || nx >= w || ny < 0 || ny >= h || Map.member next currentRocks = Map.insert curr 'O' (Map.delete coord currentRocks) -- Hit boundary or another rock
                        | otherwise = tryMove next (Coord (nx + dx) (ny + dy)) -- Keep moving
                        where nx = x next; ny = y next

                in tryMove currentPos nextPos

        finalRocks = foldl' moveRock rks orderedCoords
    in grid { rocks = finalRocks }

cycleRocks :: Grid -> Grid
cycleRocks grid =
    let north = shift grid (0, -1)
        west  = shift north (-1, 0)
        south = shift west (0, 1)
        east  = shift south (1, 0)
    in east

calculateLoad :: Grid -> Int
calculateLoad (Grid _ h rks) =
    Map.foldlWithKey' (\acc (Coord _ y) char -> if char == 'O' then acc + (h - y) else acc) 0 rks

getGridKey :: Grid -> GridKey
getGridKey = Set.fromList . Map.keys . Map.filter (== 'O') . rocks

solve :: Grid -> Int
solve initialGrid = go 0 Map.empty initialGrid
  where
    numCycles = 1000000000
    go :: Int -> Map.Map GridKey Int -> Grid -> Int
    go i cache currentGrid
      | i == numCycles = calculateLoad currentGrid
      | otherwise =
          let key = getGridKey currentGrid
          in case Map.lookup key cache of
            Just iStartCycle ->
              let cycleLen = i - iStartCycle
                  remaining = (numCycles - i) `mod` cycleLen
                  -- Apply remaining cycles efficiently using iterate
                  finalGrid = iterate cycleRocks currentGrid !! remaining
              in calculateLoad finalGrid
            Nothing ->
              let nextCache = Map.insert key i cache
                  nextGrid = cycleRocks currentGrid
              in go (i + 1) nextCache nextGrid

main :: IO ()
main = do
    inputLines <- lines <$> readFile "input.txt"
    let grid = parseInput inputLines
    print $ solve grid
