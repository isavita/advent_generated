
{-# LANGUAGE BangPatterns #-}

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (|>))
import Data.Maybe (fromJust)
import Data.List (foldl')
import System.IO (readFile)

-- Types
type Pos = (Int, Int) -- (row, col)
type Blizzard = (Pos, Char) -- ((row, col), direction)
type BlizzardState = S.Set Pos -- Set of positions occupied by blizzards at a given time
type State = (Int, Pos) -- (time, position)
type Visited = S.Set (Int, Pos) -- (time `mod` cycleLength, position)

-- Parse input
parseInput :: String -> (Int, Int, Pos, Pos, [Blizzard])
parseInput input = (maxRow, maxCol, startPos, endPos, blizzards)
  where
    ls = lines input
    maxRow = length ls - 1
    maxCol = length (head ls) - 1
    startPos = (0, fromJust $ elemIndex '.' (head ls))
    endPos = (maxRow, fromJust $ elemIndex '.' (last ls))
    blizzards = concatMap parseRow (zip [0..] ls)

    parseRow :: (Int, String) -> [Blizzard]
    parseRow (r, rowStr) = concatMap (parseCell r) (zip [0..] rowStr)

    parseCell :: Int -> (Int, Char) -> [Blizzard]
    parseCell r (c, char)
      | char `elem` "<>^v" = [((r, c), char)]
      | otherwise         = []

    elemIndex :: Eq a => a -> [a] -> Maybe Int
    elemIndex _ [] = Nothing
    elemIndex x (y:ys)
        | x == y    = Just 0
        | otherwise = fmap (+1) (elemIndex x ys)

-- Calculate blizzard positions for a given time
-- Blizzards wrap around within the inner grid (rows 1 to maxRow-1, cols 1 to maxCol-1)
moveBlizzard :: Int -> Int -> Int -> Blizzard -> Pos
moveBlizzard !maxRow !maxCol !time ((!r0, !c0), !dir) =
    let !height = maxRow - 1
        !width = maxCol - 1
        !r_inner = r0 - 1 -- 0-based inner row
        !c_inner = c0 - 1 -- 0-based inner col
    in case dir of
        '>' -> (r0, 1 + (c_inner + time) `mod` width)
        '<' -> (r0, 1 + (c_inner - time) `mod` width)
        'v' -> (1 + (r_inner + time) `mod` height, c0)
        '^' -> (1 + (r_inner - time) `mod` height, c0)
        _   -> error "Invalid blizzard direction" -- Should not happen

-- Precompute blizzard states for a full cycle
-- The cycle length is the least common multiple (LCM) of the inner grid width and height.
gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

lcm' :: Int -> Int -> Int
lcm' a b = (a * b) `div` gcd' a b

buildBlizzardCache :: Int -> Int -> [Blizzard] -> M.Map Int BlizzardState
buildBlizzardCache maxRow maxCol initialBlizzards = M.fromList $ map calcState [0..cycleLen-1]
  where
    !height = maxRow - 1
    !width = maxCol - 1
    !cycleLen = lcm' width height
    calcState :: Int -> (Int, BlizzardState)
    calcState t = (t, S.fromList $ map (moveBlizzard maxRow maxCol t) initialBlizzards)

-- Breadth-First Search
bfs :: M.Map Int BlizzardState -> Int -> Int -> Int -> Pos -> Pos -> Int -> Int
bfs blizzardCache maxRow maxCol cycleLen startPos endPos startTime = go (Seq.singleton (startTime, startPos)) S.empty
  where
    go :: Seq State -> Visited -> Int
    go Empty _ = error "No path found!" -- Should not happen in this puzzle
    go ((!currentTime, !currentPos) :<| queue) visited =
        if currentPos == endPos then
            --trace ("Reached end at time " ++ show currentTime) $
            currentTime
        else
            let -- Key for visited set uses time modulo cycle length
                !visitedKeyTime = currentTime `mod` cycleLen
                !visitedKey = (visitedKeyTime, currentPos)

                -- If already visited this state (pos at equivalent time in cycle), skip
                !visited' = S.insert visitedKey visited
                
                -- Calculate time for next step and get corresponding blizzard state
                !nextTime = currentTime + 1
                !blizzardPositions = blizzardCache M.! (nextTime `mod` cycleLen)

                -- Generate possible next moves (including waiting)
                !moves = [(0, 0), (0, 1), (0, -1), (1, 0), (-1, 0)] -- Wait, R, L, D, U
                !possibleNextPos = map (\(dr, dc) -> (fst currentPos + dr, snd currentPos + dc)) moves

                -- Filter valid next positions
                isValid :: Pos -> Bool
                isValid nextPos@(r, c)
                    | nextPos == endPos || nextPos == startPos = True -- Goal/Start always valid (except for blizzards)
                    | r <= 0 || r >= maxRow || c <= 0 || c >= maxCol = False -- Out of bounds (inner walls)
                    | otherwise = True                                  -- Inside grid

                isNotBlizzard :: Pos -> Bool
                isNotBlizzard pos = not $ S.member pos blizzardPositions

                isNotVisited :: Pos -> Bool
                isNotVisited pos = not $ S.member (nextTime `mod` cycleLen, pos) visited

                validNextStates =
                    [ (nextTime, nextPos)
                    | nextPos <- possibleNextPos
                    , isValid nextPos
                    , isNotBlizzard nextPos
                    , isNotVisited nextPos
                    ]

                -- Add new valid states to queue and visited set
                (newQueue, finalVisited) = foldl' addState (queue, visited') validNextStates

                addState :: (Seq State, Visited) -> State -> (Seq State, Visited)
                addState (q, v) state@(_, pos) =
                    let vKey = (fst state `mod` cycleLen, pos)
                    in if S.member vKey v
                       then (q, v) -- Already visited or added in this step
                       else (q |> state, S.insert vKey v)

            in go newQueue finalVisited


-- Main function
main :: IO ()
main = do
    input <- readFile "input.txt"
    let (maxRow, maxCol, startPos, endPos, initialBlizzards) = parseInput input
    let !height = maxRow - 1
    let !width = maxCol - 1
    let !cycleLen = lcm' width height
    -- putStrLn $ "Dimensions: " ++ show (maxRow, maxCol)
    -- putStrLn $ "Inner Dimensions: " ++ show (height, width)
    -- putStrLn $ "Start: " ++ show startPos
    -- putStrLn $ "End: " ++ show endPos
    -- putStrLn $ "Cycle Length: " ++ show cycleLen
    -- putStrLn $ "Number of blizzards: " ++ show (length initialBlizzards)

    let blizzardCache = buildBlizzardCache maxRow maxCol initialBlizzards
    -- putStrLn "Blizzard cache built."

    -- Part 1: Start -> End
    let time1 = bfs blizzardCache maxRow maxCol cycleLen startPos endPos 0
    putStrLn $ "Part 1: " ++ show time1

    -- Part 2: End -> Start
    let time2 = bfs blizzardCache maxRow maxCol cycleLen endPos startPos time1
    -- putStrLn $ "Time to go back: " ++ show (time2 - time1)

    -- Part 2: Start -> End (again)
    let time3 = bfs blizzardCache maxRow maxCol cycleLen startPos endPos time2
    -- putStrLn $ "Time for final leg: " ++ show (time3 - time2)

    putStrLn $ "Part 2: " ++ show time3
