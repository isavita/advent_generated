
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad (guard)

type Coord = (Int, Int)

data Grid = Grid {
    gridData :: M.Map Coord Char,
    gridWidth :: Int,
    gridHeight :: Int
} deriving Show

type Direction = (Int, Int)

north, south, west, east :: Direction
north = (0, -1)
south = (0,  1)
west  = (-1, 0)
east  = ( 1, 0)

allDirections :: [Direction]
allDirections = [north, south, west, east]

addCoord :: Coord -> Direction -> Coord
addCoord (x, y) (dx, dy) = (x + dx, y + dy)

slopeToOutgoingDir :: Char -> Maybe Direction
slopeToOutgoingDir '^' = Just north
slopeToOutgoingDir 'v' = Just south
slopeToOutgoingDir '<' = Just west
slopeToOutgoingDir '>' = Just east
slopeToOutgoingDir _   = Nothing

isInBounds :: Grid -> Coord -> Bool
isInBounds grid (x, y) = x >= 0 && x < gridWidth grid && y >= 0 && y < gridHeight grid

parseInput :: String -> Grid
parseInput content =
    let ls = lines content
        h = length ls
        w = if h > 0 then length (head ls) else 0
        coords = [ ((x, y), char)
                 | (y, line) <- zip [0..] ls
                 , (x, char) <- zip [0..] line
                 , char /= '.' ]
    in Grid (M.fromList coords) w h

isValidNeighborSimple :: Grid -> Coord -> Direction -> Bool
isValidNeighborSimple grid neighbor _dir =
    isInBounds grid neighbor && M.lookup neighbor (gridData grid) /= Just '#'

isValidNeighborWithSlopes :: Grid -> Coord -> Direction -> Bool
isValidNeighborWithSlopes grid neighbor dirToNeighbor =
    isInBounds grid neighbor &&
    case M.lookup neighbor (gridData grid) of
        Nothing -> True
        Just '#' -> False
        Just c   -> case slopeToOutgoingDir c of
                        Nothing -> True -- Should be '.' implicitly, but wasn't stored. Treat as path.
                        Just allowedDir -> dirToNeighbor == allowedDir

neighbors4 :: Grid -> Coord -> (Grid -> Coord -> Direction -> Bool) -> [Coord]
neighbors4 grid current isValidFn =
    [ next | dir <- allDirections
           , let next = addCoord current dir
           , isValidFn grid next dir ]

type Graph = M.Map Coord (M.Map Coord Int)

getGraph :: Grid -> Coord -> Coord -> (Grid -> Coord -> Direction -> Bool) -> Graph
getGraph grid start end isValidEdgeFn =
    let potentialJunctions = S.fromList
            [ coord
            | y <- [0..gridHeight grid - 1]
            , x <- [0..gridWidth grid - 1]
            , let coord = (x, y)
            , M.lookup coord (gridData grid) /= Just '#'
            , length (neighbors4 grid coord isValidNeighborSimple) > 2
            ]
        vertices = S.insert start $ S.insert end potentialJunctions
        edgeMapList = [ (v, getEdgesBFS grid v vertices isValidEdgeFn) | v <- S.toList vertices ]
    in M.fromList edgeMapList

getEdgesBFS :: Grid -> Coord -> S.Set Coord -> (Grid -> Coord -> Direction -> Bool) -> M.Map Coord Int
getEdgesBFS grid start vertices isValidEdgeFn = go (Seq.singleton (start, 0)) (M.singleton start 0) M.empty
  where
    go queue dists edgesFound =
        case Seq.viewl queue of
            Seq.EmptyL -> edgesFound
            (current, d) Seq.:< restQueue ->
                let processNeighbor (currentEdges, currentQueue, currentDists) next =
                         if M.notMember next currentDists then
                            let newD = d + 1
                            in if S.member next vertices then
                                   (M.insert next newD currentEdges, currentQueue, M.insert next newD currentDists)
                               else
                                   (currentEdges, currentQueue Seq.|> (next, newD), M.insert next newD currentDists)
                         else
                            (currentEdges, currentQueue, currentDists)

                    neighbors = neighbors4 grid current isValidEdgeFn
                    (newEdges, furtherQueue, newDists) = foldl processNeighbor (edgesFound, restQueue, dists) neighbors
                in go furtherQueue newDists newEdges

getMaxDistanceDFS :: Graph -> Coord -> Coord -> S.Set Coord -> Maybe Int
getMaxDistanceDFS graph current end visited =
    if current == end then
        Just 0
    else
        let visited' = S.insert current visited
            neighbors = M.findWithDefault M.empty current graph

            results = mapMaybe processNeighbor (M.toList neighbors)

            processNeighbor (next, dist) =
                if S.member next visited' then
                    Nothing
                else
                    case getMaxDistanceDFS graph next end visited' of
                        Nothing -> Nothing
                        Just subDist -> Just (dist + subDist)

        in if null results then Nothing else Just (maximum results)

solve :: String -> Int
solve input =
    let grid = parseInput input
        start = (1, 0)
        end = (gridWidth grid - 2, gridHeight grid - 1)
        graph = getGraph grid start end isValidNeighborWithSlopes
        maxDist = getMaxDistanceDFS graph start end S.empty
    in fromMaybe 0 maxDist

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ solve input
