
import Data.Char (isDigit)
import Data.List (maximum)
import qualified Text.ParserCombinators.ReadP as P

-- Type alias for clarity
type TargetArea = (Int, Int, Int, Int) -- (xmin, xmax, ymin, ymax)
type Point = (Int, Int)
type Velocity = (Int, Int)
type State = (Int, Int, Int, Int) -- (x, y, vx, vy)

-- --- Input Parsing ---

-- Parses an integer (positive or negative)
parseInt :: P.ReadP Int
parseInt = read <$> P.munch1 (\c -> isDigit c || c == '-')

-- Parses the target area format "x=A..B, y=C..D"
targetParser :: P.ReadP TargetArea
targetParser = do
  _ <- P.string "x="
  xmin <- parseInt
  _ <- P.string ".."
  xmax <- parseInt
  _ <- P.string ", y="
  ymin <- parseInt
  _ <- P.string ".."
  ymax <- parseInt
  P.eof -- Ensure the whole string is consumed
  return (xmin, xmax, ymin, ymax)

-- Function to parse the relevant part of the input file content
parseTargetArea :: String -> Maybe TargetArea
parseTargetArea s =
  -- Assumes input format "target area: x=A..B, y=C..D"
  let prefix = "target area: "
      targetStr = drop (length prefix) s
  in case P.readP_to_S targetParser targetStr of
    [(area, "")] -> Just area -- Successful parse
    _            -> Nothing   -- Failed parse

-- --- Simulation Logic ---

-- Performs one step of the simulation
step :: State -> State
step (x, y, vx, vy) = (x', y', vx', vy')
  where
    x' = x + vx
    y' = y + vy
    -- Drag: vx changes towards 0
    vx' = if vx == 0 then 0 else vx - signum vx
    -- Gravity: vy decreases by 1
    vy' = vy - 1

-- Checks if a given point is within the target area
isInTarget :: TargetArea -> Point -> Bool
isInTarget (xmin, xmax, ymin, ymax) (x, y) =
    x >= xmin && x <= xmax && y >= ymin && y <= ymax

-- Simulates the trajectory for a given initial velocity and checks if it *ever* hits the target.
-- Returns True if it hits, False otherwise.
doesHit :: TargetArea -> Velocity -> Bool
doesHit target@(xmin, xmax, ymin, ymax) (initialVx, initialVy) =
    -- Start simulation from the state *after* the first step (position is not (0,0))
    go (step (0, 0, initialVx, initialVy))
  where
    -- Recursive helper function to check trajectory steps
    go :: State -> Bool
    go state@(x, y, vx, vy)
      -- Check if current position is in the target
      | isInTarget target (x, y) = True
      -- Check termination conditions (probe is definitely past the target)
      | x > xmax                 = False -- Gone too far right
      | y < ymin                 = False -- Gone too low (and only going lower)
      | vx == 0 && x < xmin      = False -- Stopped horizontally before reaching target x-range
      -- Continue simulation if not hit and not past target
      | otherwise                = go (step state)

-- --- Calculation Logic ---

-- Calculates the maximum y-position reached for a given initial vertical velocity (vy).
-- Formula for sum of 1 to n: n*(n+1)/2. Only applies if vy > 0.
calculateMaxY :: Int -> Int
calculateMaxY vy = if vy > 0 then vy * (vy + 1) `div` 2 else 0

-- Finds the smallest initial integer vx > 0 such that the probe *could* reach xmin,
-- considering drag stops the horizontal movement eventually.
-- The max distance reached is vx*(vx+1)/2. We need this >= xmin.
findMinVx :: Int -> Int
findMinVx xmin = head $ dropWhile (\vx -> vx * (vx + 1) `div` 2 < xmin) [1..]

-- --- Main Program ---

main :: IO ()
main = do
    -- Read input from file
    content <- readFile "input.txt"

    -- Parse the target area from the input content
    case parseTargetArea (head $ lines content) of -- Assume first line contains the target area
        Nothing -> putStrLn "Error: Could not parse input.txt"
        Just target@(xmin, xmax, ymin, ymax) -> do

            -- Determine the search range for initial velocities (vx, vy)
            -- vx range:
            -- Min: Must be able to reach xmin eventually (findMinVx)
            -- Max: Must not overshoot xmax on the first step (xmax)
            let minVx = findMinVx xmin
            let maxVx = xmax

            -- vy range:
            -- Min: Must not undershoot ymin on the first step (ymin)
            -- Max: When coming down, the step *after* passing y=0 has y = -(initialVy + 1).
            --      This must be >= ymin, so -(initialVy + 1) >= ymin => initialVy + 1 <= -ymin => initialVy <= -ymin - 1.
            --      Any higher initial vy will overshoot downwards.
            let minVy = ymin
            let maxVy = -ymin - 1 -- Optimization: derived upper bound

            -- Generate all candidate initial velocities within the calculated ranges
            let candidateVelocities = [(vx, vy) | vx <- [minVx..maxVx], vy <- [minVy..maxVy]]

            -- Filter the velocities to find those that hit the target area
            let hittingVelocities = filter (doesHit target) candidateVelocities

            -- For each hitting velocity, calculate the maximum y-position achieved during its trajectory
            let maxYPositions = map (calculateMaxY . snd) hittingVelocities

            -- Find the highest y-position among all successful trajectories
            -- Handle the case where no hitting velocity is found (though the problem implies one exists)
            let result = if null maxYPositions then 0 else maximum maxYPositions

            -- Print the result to standard output
            print result

