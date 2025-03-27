
{-# LANGUAGE StrictData #-}
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.IO (readFile)
import Control.Monad (guard)

data Coord = Coord { x :: !Int, y :: !Int } deriving (Eq, Ord, Show)

add :: Coord -> Coord -> Coord
(Coord x1 y1) `add` (Coord x2 y2) = Coord (x1 + x2) (y1 + y2)

opposite :: Coord -> Coord
opposite (Coord x y) = Coord (-x) (-y)

type Tile = Char
type Grid = M.Map Coord Tile
type Pipe = S.Set Coord

top, right, bottom, left :: Coord
top    = Coord 0 (-1)
right  = Coord 1 0
bottom = Coord 0 1
left   = Coord (-1) 0

allDirs :: [Coord]
allDirs = [top, right, bottom, left]

getPipeFromTile :: Tile -> Pipe
getPipeFromTile '|' = S.fromList [top, bottom]
getPipeFromTile '-' = S.fromList [left, right]
getPipeFromTile 'J' = S.fromList [top, left]
getPipeFromTile 'L' = S.fromList [top, right]
getPipeFromTile '7' = S.fromList [bottom, left]
getPipeFromTile 'F' = S.fromList [bottom, right]
getPipeFromTile _   = S.empty

getTileFromPipe :: Pipe -> Tile
getTileFromPipe pipe
    | pipe == getPipeFromTile '|' = '|'
    | pipe == getPipeFromTile '-' = '-'
    | pipe == getPipeFromTile 'J' = 'J'
    | pipe == getPipeFromTile 'L' = 'L'
    | pipe == getPipeFromTile '7' = '7'
    | pipe == getPipeFromTile 'F' = 'F'
    | otherwise                 = '.'

buildGrid :: [String] -> Grid
buildGrid ls = M.fromList $ do
    (y, line) <- zip [0..] ls
    (x, char) <- zip [0..] line
    -- Store all tiles including '.' initially, needed for dimensions later
    return (Coord x y, char)

findStart :: Grid -> Coord
findStart grid = fst . head . M.toList $ M.filter (== 'S') grid

getStartPipe :: Coord -> Grid -> Pipe
getStartPipe startCoord grid = S.fromList $ do
    dir <- allDirs
    let neighborCoord = startCoord `add` dir
    case M.lookup neighborCoord grid of
        Just neighborTile -> do
            let neighborPipe = getPipeFromTile neighborTile
            guard (opposite dir `S.member` neighborPipe)
            return dir
        Nothing -> []

pathFinding :: Coord -> Grid -> [Coord]
pathFinding start grid = start : go start initialDir initialPos
  where
    startPipe = getStartPipe start grid
    initialDir = S.elemAt 0 startPipe
    initialPos = start `add` initialDir

    go :: Coord -> Coord -> Coord -> [Coord]
    go prevPos prevDir currentPos
        | currentPos == start = []
        | otherwise =
            case M.lookup currentPos grid of
                Just currentTile ->
                    let currentPipe = getPipeFromTile currentTile
                        nextDirSet = S.delete (opposite prevDir) currentPipe
                    in if S.null nextDirSet then [] -- Should not happen in valid loop
                       else let nextDir = S.elemAt 0 nextDirSet
                                nextPos = currentPos `add` nextDir
                            in currentPos : go currentPos nextDir nextPos
                Nothing -> [] -- Should not happen in valid loop

getPathGrid :: Grid -> [Coord] -> Grid
getPathGrid grid path =
    let startCoord = head path
        startPipeType = getStartPipe startCoord grid
        startTile = getTileFromPipe startPipeType
        pathSet = S.fromList path
        -- Keep only path tiles, replace 'S'
        finalGrid = M.mapMaybeWithKey (\k v -> if k `S.member` pathSet
                                                then Just $ if v == 'S' then startTile else v
                                                else Nothing) grid
    in finalGrid


countEnclosed :: Int -> Int -> Grid -> Int
countEnclosed width height pathGrid = sum $ map processRow [0..height-1]
  where
    processRow :: Int -> Int
    processRow y = go 0 False 0 Nothing 0 -- x, isInside, crossings, lastCorner, rowCount
      where
        go :: Int -> Bool -> Int -> Maybe Tile -> Int -> Int
        go x currentInside crossings lastCorner count
          | x >= width = count
          | otherwise =
              let coord = Coord x y
                  (newInside, newCrossings, newLastCorner, newCount) =
                      case M.lookup coord pathGrid of
                          -- Tile is part of the pipe loop
                          Just tile ->
                              let (updatedCrossings, updatedLastCorner) = updateState tile crossings lastCorner
                                  updatedInside = odd updatedCrossings
                              in (updatedInside, updatedCrossings, updatedLastCorner, count)
                          -- Tile is NOT part of the loop
                          Nothing ->
                              let increment = if currentInside then 1 else 0
                              -- Reset lastCorner when passing through non-pipe area
                              in (currentInside, crossings, Nothing, count + increment)

              in go (x + 1) newInside newCrossings newLastCorner newCount

    -- Updates crossing count based on current tile and last corner seen
    updateState :: Tile -> Int -> Maybe Tile -> (Int, Maybe Tile)
    updateState '|' crossings _          = (crossings + 1, Nothing)
    updateState '-' crossings lastCorner = (crossings, lastCorner)
    updateState 'L' crossings _          = (crossings, Just 'L')
    updateState 'F' crossings _          = (crossings, Just 'F')
    updateState 'J' crossings lastCorner = case lastCorner of
                                             Just 'F' -> (crossings + 1, Nothing) -- F---J
                                             _        -> (crossings, Nothing)     -- L---J or ---J
    updateState '7' crossings lastCorner = case lastCorner of
                                             Just 'L' -> (crossings + 1, Nothing) -- L---7
                                             _        -> (crossings, Nothing)     -- F---7 or ---7
    updateState _   crossings lastCorner = (crossings, lastCorner) -- Should not happen

solve :: [String] -> Int
solve inputLines =
    let grid = buildGrid inputLines
        start = findStart grid
        path = pathFinding start grid
        pathGrid = getPathGrid grid path
        height = length inputLines
        width = if height > 0 then length (head inputLines) else 0
    in countEnclosed width height pathGrid

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let inputLines = lines contents
    print $ solve inputLines

