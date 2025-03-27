
{-# LANGUAGE TupleSections #-}
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (foldl')

type Coord = (Int, Int)

-- Parses a single line of directions into a final coordinate
parseLine :: String -> Coord
parseLine = go (0, 0)
  where
    go :: Coord -> String -> Coord
    go coord [] = coord
    go (x, y) ('s':'e':rest) = go (x, y - 1) rest
    go (x, y) ('s':'w':rest) = go (x - 1, y - 1) rest
    go (x, y) ('n':'w':rest) = go (x, y + 1) rest
    go (x, y) ('n':'e':rest) = go (x + 1, y + 1) rest
    go (x, y) ('e':rest)     = go (x + 1, y) rest
    go (x, y) ('w':rest)     = go (x - 1, y) rest
    go _ other = error ("Invalid input: " ++ other)

-- Calculates the initial set of black tiles by flipping
initialTiles :: [String] -> S.Set Coord
initialTiles ls = M.keysSet . M.filter odd $ foldl' countCoord M.empty (map parseLine ls)
  where
    countCoord :: M.Map Coord Int -> Coord -> M.Map Coord Int
    countCoord counts coord = M.insertWith (+) coord 1 counts

-- Offsets for adjacent hex tiles
adjOffsets :: [Coord]
adjOffsets = [(1, 0), (0, -1), (-1, -1), (-1, 0), (0, 1), (1, 1)]

-- Get coordinates of adjacent tiles
getAdjacent :: Coord -> [Coord]
getAdjacent (x, y) = [(x + dx, y + dy) | (dx, dy) <- adjOffsets]

-- Count black adjacent tiles
countAdjacent :: S.Set Coord -> Coord -> Int
countAdjacent blackTiles coord =
    length $ filter (`S.member` blackTiles) (getAdjacent coord)

-- Perform one simulation step
step :: S.Set Coord -> S.Set Coord
step blackTiles = S.filter staysAlive candidates
  where
    -- Tiles to consider: black tiles and their neighbors
    candidates :: S.Set Coord
    candidates = blackTiles `S.union` S.unions (S.map (S.fromList . getAdjacent) blackTiles)

    -- Rule for a tile to be black in the next generation
    staysAlive :: Coord -> Bool
    staysAlive coord =
      let isBlack = S.member coord blackTiles
          adjCount = countAdjacent blackTiles coord
      in if isBlack
         then adjCount == 1 || adjCount == 2 -- Black stays black if 1 or 2 neighbors
         else adjCount == 2                 -- White becomes black if exactly 2 neighbors

-- Run the simulation for n steps
simulate :: Int -> S.Set Coord -> S.Set Coord
simulate n initial = iterate step initial !! n

main :: IO ()
main = do
    input <- readFile "input.txt"
    let ls = lines input
    let initial = initialTiles ls
    let finalTiles = simulate 100 initial
    print (S.size finalTiles)
