
import qualified Data.Set as Set
import Data.List (foldl')

type Point = (Int, Int)
type Rope = [Point]
type VisitedSet = Set.Set Point
type Move = (Char, Int)

-- | Calculate the change in coordinates for a given direction character.
dirDelta :: Char -> Point
dirDelta 'U' = (0, 1)
dirDelta 'D' = (0, -1)
dirDelta 'L' = (-1, 0)
dirDelta 'R' = (1, 0)
-- Handle alternative direction characters from the Python example, though not strictly needed by problem descriptions usually
dirDelta 'N' = (0, 1)
dirDelta 'S' = (0, -1)
dirDelta 'W' = (-1, 0)
dirDelta 'E' = (1, 0)
dirDelta '^' = (0, 1)
dirDelta 'v' = (0, -1)
dirDelta '<' = (-1, 0)
dirDelta '>' = (1, 0)
dirDelta _   = (0, 0) -- Default case, might indicate an error in input

-- | Add two points component-wise.
addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- | Subtract two points component-wise.
subPoints :: Point -> Point -> Point
subPoints (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- | Calculate the next position of a following knot based on the leading knot.
nextKnotPos :: Point -> Point -> Point
nextKnotPos headPos tailPos =
    let (dx, dy) = subPoints headPos tailPos
    in if abs dx <= 1 && abs dy <= 1
       then tailPos -- Stay if touching or overlapping
       else addPoints tailPos (signum dx, signum dy) -- Move one step towards head

-- | Update the entire rope for a single step move of the head.
-- The head moves by 'delta', and subsequent knots follow using 'nextKnotPos'.
-- Uses scanl to efficiently compute the new positions iteratively.
stepRopeOnce :: Point -> Rope -> Rope
stepRopeOnce delta rope =
    case rope of
        [] -> []
        (h:t) -> let newHead = addPoints h delta
                 in scanl nextKnotPos newHead t

-- | State for the simulation, containing the current rope state and the set of visited tail positions.
type SimState = (Rope, VisitedSet)

-- | Perform a single step of the simulation for a given direction.
performStep :: Char -> SimState -> SimState
performStep dir (rope, visited) =
    let delta = dirDelta dir
        newRope = stepRopeOnce delta rope
        -- Ensure rope is not empty before accessing last element
        newVisited = case reverse newRope of
                        (lastKnot:_) -> Set.insert lastKnot visited
                        []           -> visited -- Should not happen with non-zero ropeLen
    in (newRope, newVisited)

-- | Perform a full move instruction (direction and number of steps).
performMove :: Move -> SimState -> SimState
performMove (dir, n) state = foldl' (\s _ -> performStep dir s) state [1..n]

-- | Parse the input string into a list of moves.
parseInput :: String -> [Move]
parseInput = map parseLine . lines
  where
    parseLine line = case words line of
                       [dirStr, numStr] | not (null dirStr) -> (head dirStr, read numStr)
                       _ -> error $ "Invalid input line: " ++ line -- Basic error handling

-- | Run the simulation with a given rope length and list of moves.
runSimulation :: Int -> [Move] -> Int
runSimulation ropeLen moves =
    let initialRope = replicate ropeLen (0, 0)
        initialVisited = Set.singleton (0, 0)
        initialState = (initialRope, initialVisited)
        -- Use foldl' with flipped performMove to process moves sequentially
        (_, finalVisited) = foldl' (flip performMove) initialState moves
    in Set.size finalVisited

-- | Main entry point. Reads input, parses, simulates, and prints the result.
main :: IO ()
main = do
    input <- readFile "input.txt"
    let moves = parseInput input
    let ropeLen = 10 -- As specified in the original problem/Python code
    print $ runSimulation ropeLen moves
