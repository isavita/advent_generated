
import qualified Data.Array as A
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad (guard)

-- Represents the state: position (row, col), direction moved into this cell (dr, dc), consecutive steps in that direction
-- Using strictness annotations (!) for potentially better performance
data State = State !(Int, Int) !(Int, Int) !Int deriving (Eq, Ord, Show)

-- Define directions
directions :: [(Int, Int)]
directions = [(0, 1), (1, 0), (0, -1), (-1, 0)] -- Right, Down, Left, Up

-- Function to parse the input file into a 2D Array
parseInput :: String -> A.Array (Int, Int) Int
parseInput input = 
    let ls = lines input
        rows = length ls
        cols = if rows > 0 then length (head ls) else 0
        bounds = ((0, 0), (rows - 1, cols - 1))
        assocs = [ ((r, c), read [digit]) 
                 | (r, rowStr) <- zip [0..] ls
                 , (c, digit) <- zip [0..] rowStr ]
    in A.array bounds assocs

-- Dijkstra's algorithm implementation
solve :: A.Array (Int, Int) Int -> Int
solve grid =
    let bounds@((minR, minC), (maxR, maxC)) = A.bounds grid
        target = (maxR, maxC)

        -- Priority Queue: Map from cost to a Set of States at that cost
        -- Using Map for cost keys and Set for states to handle multiple states with the same cost efficiently.
        pqInitial :: M.Map Int (S.Set State)
        pqInitial = M.empty 

        -- Distance Map: Stores the minimum cost found so far to reach a specific State
        distInitial :: M.Map State Int
        distInitial = M.empty

        -- Add initial states to PQ and Distances
        -- We can start by moving Right or Down from (0,0)
        -- The cost incurred is that of the cell entered.
        addInitialState :: (Int, Int) -> (Int, Int) -> (M.Map Int (S.Set State), M.Map State Int) -> (M.Map Int (S.Set State), M.Map State Int)
        addInitialState startPos nextDir (pq, dist) =
            let nextPos@(nr, nc) = (fst startPos + fst nextDir, snd startPos + snd nextDir)
            in if A.inRange bounds nextPos then
                   let cost = grid A.! nextPos
                       initialState = State nextPos nextDir 1 -- First step in this direction
                       newPQ = M.insertWith S.union cost (S.singleton initialState) pq
                       newDist = M.insert initialState cost dist
                   in (newPQ, newDist)
               else (pq, dist) -- Should not happen if grid is non-empty

        (pqStart, distStart) = foldl (flip ($)) (pqInitial, distInitial) 
            [ addInitialState (0,0) (0,1) -- Initial move Right
            , addInitialState (0,0) (1,0) -- Initial move Down
            ]

        -- Dijkstra loop
        dijkstra :: M.Map Int (S.Set State) -> M.Map State Int -> Int
        dijkstra pq dist
            | M.null pq = error "Target not reachable" -- Should not happen in this problem
            | otherwise =
                -- Extract minimum cost element
                let ((minCost, states), pq') = M.deleteFindMin pq
                    -- Process one state from the set of minimum cost states
                    (currentState@(State pos@(r, c) dir cons), remainingStates) = S.deleteFindMin states
                    pq'' = if S.null remainingStates then pq' else M.insert minCost remainingStates pq'
                in
                    -- Check if this state has already been finalized with a lower cost (important if we add states multiple times)
                    if Just minCost > M.lookup currentState dist then 
                        dijkstra pq'' dist -- Already found a shorter path, skip
                    else if pos == target then
                        minCost -- Found the target
                    else
                        -- Explore neighbors
                        let neighbors = findNeighbors currentState
                            (finalPQ, finalDist) = foldl (processNeighbor minCost) (pq'', dist) neighbors
                        in dijkstra finalPQ finalDist
            where
                -- Function to find valid neighboring states
                findNeighbors :: State -> [(State, Int)]
                findNeighbors (State (r, c) (dr, dc) cons) = do
                    (nr, nc) <- directions -- Try all four directions
                    
                    -- Constraint: Cannot reverse direction
                    guard ( (nr, nc) /= (-dr, -dc) ) 

                    let nextPos = (r + nr, c + nc)
                    
                    -- Check bounds
                    guard (A.inRange bounds nextPos) 

                    let nextCons = if (nr, nc) == (dr, dc) then cons + 1 else 1
                    
                    -- Constraint: Max 3 steps in the same direction
                    guard ( nextCons <= 3 )

                    let nextState = State nextPos (nr, nc) nextCons
                    let costToEnter = grid A.! nextPos
                    
                    return (nextState, costToEnter)

                -- Function to process a potential neighbor state
                processNeighbor :: Int -> (M.Map Int (S.Set State), M.Map State Int) -> (State, Int) -> (M.Map Int (S.Set State), M.Map State Int)
                processNeighbor currentCost (currentPQ, currentDist) (nextState, costToEnter) =
                    let newCost = currentCost + costToEnter
                    in case M.lookup nextState currentDist of
                        Just existingCost | existingCost <= newCost -> (currentPQ, currentDist) -- Found a path already that's not worse
                        _ -> -- Found a new shorter path or first path
                             let newDist = M.insert nextState newCost currentDist
                                 newPQ = M.insertWith S.union newCost (S.singleton nextState) currentPQ
                             in (newPQ, newDist)

    in dijkstra pqStart distStart

-- Main function to read input, solve, and print output
main :: IO ()
main = do
    input <- readFile "input.txt"
    let grid = parseInput input
    let result = solve grid
    print result
