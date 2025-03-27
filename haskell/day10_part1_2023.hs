
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.IO (readFile)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (find)

type Coord = (Int, Int)
type Tile = Char
type Grid = Map.Map Coord Tile
type Pipe = Set.Set Coord

-- Directions
top, right, bottom, left :: Coord
top    = (0, -1)
right  = (1, 0)
bottom = (0, 1)
left   = (-1, 0)

-- Tiles
start :: Tile
start = 'S'
empty :: Tile
empty = '.'

-- Tile to Pipe mapping
tileToPipeMap :: Map.Map Tile Pipe
tileToPipeMap = Map.fromList
    [ ('|', Set.fromList [top, bottom])
    , ('-', Set.fromList [left, right])
    , ('J', Set.fromList [top, left])
    , ('L', Set.fromList [top, right])
    , ('7', Set.fromList [bottom, left])
    , ('F', Set.fromList [bottom, right])
    ]

getPipeFromTile :: Tile -> Pipe
getPipeFromTile tile = Map.findWithDefault Set.empty tile tileToPipeMap

addCoord :: Coord -> Coord -> Coord
addCoord (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

negateCoord :: Coord -> Coord
negateCoord (x, y) = (-x, -y)

buildGrid :: [String] -> Grid
buildGrid inputLines = Map.fromList $ do
    (y, line) <- zip [0..] inputLines
    (x, char) <- zip [0..] line
    [( (x, y), char )] -- No need to filter empty, pathfinding handles missing keys

findStart :: Grid -> Coord
findStart grid = fst $ fromMaybe (error "Start 'S' not found") $ find ((== start) . snd) (Map.toList grid)

getStartPipe :: Coord -> Grid -> Pipe
getStartPipe coord grid = Set.fromList $ do
    dir <- [top, right, bottom, left]
    let neighborCoord = addCoord coord dir
    case Map.lookup neighborCoord grid of
        Just neighborTile -> do
            let neighborPipe = getPipeFromTile neighborTile
            if Set.member (negateCoord dir) neighborPipe
               then return dir
               else []
        Nothing -> [] -- Neighbor coord not in grid

pathFinding :: Coord -> Grid -> [Coord]
pathFinding startCoord grid =
    let startPipe = getStartPipe startCoord grid
        firstDir = fromMaybe (error "Start has no connections") (Set.lookupMin startPipe)
        firstStepCoord = addCoord startCoord firstDir

        walk :: Coord -> Coord -> [Coord]
        walk currentCoord prevDir
            | currentCoord == startCoord = []
            | otherwise =
                let currentTile = fromMaybe empty (Map.lookup currentCoord grid) -- Default to empty if somehow off path
                    currentPipe = getPipeFromTile currentTile
                    nextDir = fromMaybe (error "Dead end in pipe loop") $ Set.lookupMin $ Set.delete (negateCoord prevDir) currentPipe
                    nextCoord = addCoord currentCoord nextDir
                in currentCoord : walk nextCoord nextDir

    in startCoord : walk firstStepCoord firstDir

solve :: [String] -> Int
solve inputLines =
    let grid = buildGrid inputLines
        startCoord = findStart grid
        path = pathFinding startCoord grid
    in length path `div` 2

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let inputLines = lines contents
    print (solve inputLines)
