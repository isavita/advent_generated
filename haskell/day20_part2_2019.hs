
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (Seq ((:<|)), (|>))
import Data.Char (isUpper)
import Data.List (foldl')
import Data.Maybe (fromMaybe, mapMaybe)

type Coord = (Int, Int) -- (x, y) or (col, row)
type Grid = Map.Map Coord Char
type PortalsRaw = Map.Map String [Coord]
type PortalMap = Map.Map Coord (Coord, Bool) -- Entry -> (Destination, IsEntryOuter)
type BFSState = (Coord, Int) -- ((x, y), level)
type QueueItem = (BFSState, Int) -- (state, steps)
type VisitedSet = Set.Set BFSState

-- Read input and parse into a grid map
parseGrid :: String -> Grid
parseGrid contents = Map.fromList
    [ ((x, y), c)
    | (y, row) <- zip [0..] (lines contents)
    , (x, c) <- zip [0..] row
    , c /= ' ' -- Ignore spaces for efficiency
    ]

-- Find portal labels and their entry coordinates '.'
findPortals :: Grid -> (PortalsRaw, Int, Int)
findPortals grid = (portalsMap, width, height)
  where
    coords = Map.keys grid
    maxX = maximum $ map fst coords
    maxY = maximum $ map snd coords
    width = maxX + 1 -- Add 1 because coords are 0-indexed
    height = maxY + 1

    lookupCoord c = Map.lookup c grid

    potentialPortals = mapMaybe processCoord (Map.toList grid)

    processCoord :: (Coord, Char) -> Maybe (String, Coord)
    processCoord (pos@(x, y), c1)
      | isUpper c1 = checkNeighbors pos c1
      | otherwise = Nothing

    checkNeighbors :: Coord -> Char -> Maybe (String, Coord)
    checkNeighbors pos@(x, y) c1 =
        -- Check horizontal (right)
        (case lookupCoord (x + 1, y) of
            Just c2 | isUpper c2 -> findAdjacent '.' [(x + 2, y), (x - 1, y)] [c1, c2]
            _ -> Nothing)
        `orElse`
        -- Check vertical (down)
        (case lookupCoord (x, y + 1) of
            Just c2 | isUpper c2 -> findAdjacent '.' [(x, y + 2), (x, y - 1)] [c1, c2]
            _ -> Nothing)

    findAdjacent :: Char -> [Coord] -> String -> Maybe (String, Coord)
    findAdjacent targetChar potentialPos label =
        case filter (\p -> lookupCoord p == Just targetChar) potentialPos of
            (entryPoint:_) -> Just (label, entryPoint)
            [] -> Nothing

    portalsMap = Map.fromListWith (++) [(lbl, [coord]) | (lbl, coord) <- potentialPortals]

    orElse :: Maybe a -> Maybe a -> Maybe a
    orElse (Just a) _ = Just a
    orElse Nothing b = b

-- Determine if a portal coordinate is on the outer edge
isOuter :: Coord -> Int -> Int -> Bool
isOuter (x, y) width height = x <= 2 || y <= 2 || x >= width - 3 || y >= height - 3

-- Build the mapping between portal entry points
buildPortalMapping :: PortalsRaw -> Int -> Int -> (Coord, Coord, PortalMap)
buildPortalMapping portalsRaw width height = (start, end, mapping)
  where
    start = head $ fromMaybe [] (Map.lookup "AA" portalsRaw)
    end   = head $ fromMaybe [] (Map.lookup "ZZ" portalsRaw)

    portalPairs = Map.toList $ Map.filter (\lst -> length lst == 2) $ Map.delete "AA" $ Map.delete "ZZ" portalsRaw

    mapping = Map.fromList $ concatMap processPair portalPairs

    processPair :: (String, [Coord]) -> [(Coord, (Coord, Bool))]
    processPair (_, [p1, p2]) =
        let p1Outer = isOuter p1 width height
            p2Outer = isOuter p2 width height
        in [(p1, (p2, p1Outer)), (p2, (p1, p2Outer))]
    processPair _ = [] -- Should not happen with filtered list

-- Breadth-First Search on recursive levels
bfs :: Grid -> Coord -> Coord -> PortalMap -> Maybe Int
bfs grid start end portalMap = go initialQueue initialVisited
  where
    initialState = (start, 0) -- ((x, y), level)
    initialQueue = Seq.singleton (initialState, 0) -- (state, steps)
    initialVisited = Set.singleton initialState
    targetCoord = end

    go :: Seq.Seq QueueItem -> VisitedSet -> Maybe Int
    go queue visited
      | Seq.null queue = Nothing -- Not found
      | otherwise =
          let ((currentCoord@(x, y), level), steps) :<| restQueue = queue
          in if currentCoord == targetCoord && level == 0
             then Just steps
             else
               let -- 1. Adjacent walkable cells ('.')
                   neighbors =
                       [ ((nx, ny), level)
                       | (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)]
                       , let nx = x + dx, let ny = y + dy
                       , Map.lookup (nx, ny) grid == Just '.'
                       ]

                   -- 2. Portal transitions
                   portalTransitions = case Map.lookup currentCoord portalMap of
                       Just (destCoord, isEntryOuter) ->
                           let newLevel = if isEntryOuter then level - 1 else level + 1
                           in if newLevel >= 0 then [(destCoord, newLevel)] else []
                       Nothing -> []

                   -- Combine possible next states
                   nextStates = map (, steps + 1) (neighbors ++ portalTransitions)

                   -- Add valid, unvisited states to queue and visited set
                   (newQueue, newVisited) = foldl' processState (restQueue, visited) nextStates

               in go newQueue newVisited

    processState :: (Seq.Seq QueueItem, VisitedSet) -> QueueItem -> (Seq.Seq QueueItem, VisitedSet)
    processState (q, v) item@(state, _) =
      if Set.member state v
      then (q, v)
      else (q |> item, Set.insert state v)

-- Main entry point
main :: IO ()
main = do
    contents <- readFile "input.txt"
    let grid = parseGrid contents
    let (portalsRaw, width, height) = findPortals grid
    case (Map.lookup "AA" portalsRaw, Map.lookup "ZZ" portalsRaw) of
        (Just (start:_), Just (end:_)) -> do
            let (_, _, portalMap) = buildPortalMapping portalsRaw width height
            case bfs grid start end portalMap of
                Just result -> print result
                Nothing -> putStrLn "No solution found"
        _ -> putStrLn "Error: Start (AA) or End (ZZ) portal not found."

