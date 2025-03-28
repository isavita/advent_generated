
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Char (isDigit)
import Data.List (foldl', span, break)
import Data.Maybe (fromMaybe)

data P = P { x :: !Int, y :: !Int } deriving (Eq, Ord, Show)
data Dir = N | E | S | W deriving (Eq, Enum, Bounded, Show)
data Movement = Steps !Int | Rotate !Char deriving (Show)
data Human = Human { curr :: !P, facing :: !Dir } deriving (Show)

rotate :: Dir -> Char -> Dir
rotate dir 'R' = toEnum $ (fromEnum dir + 1) `mod` 4
rotate dir 'L' = toEnum $ (fromEnum dir - 1 + 4) `mod` 4
rotate dir _   = dir

points :: Dir -> Int
points dir = (fromEnum dir + 3) `mod` 4

dirDelta :: Dir -> P
dirDelta N = P (-1) 0
dirDelta E = P 0 1
dirDelta S = P 1 0
dirDelta W = P 0 (-1)

crossBorder :: P -> Dir -> Int -> (P, Dir)
crossBorder (P nx ny) dir size
    | nx == -1    && ny <  2 * s = (P (ny + 2 * s) (nx + 1)      , E) -- 1 -> 6
    | nx == -1    && ny >= 2 * s = (P (nx + 4 * s) (ny - 2 * s)  , N) -- 2 -> 6
    | nx == s     && dir == S    = (P (ny - s)     (nx + s - 1)  , W) -- 3 -> 4
    | nx == 2*s-1 && dir == N    = (P (ny + s)     (nx - s + 1)  , E) -- 5 -> 4
    | nx == 3*s   && dir == S    = (P (ny + 2 * s) (nx - 2*s - 1), W) -- 6 -> 2
    | nx == 4*s                 = (P (nx - 4 * s) (ny + 2 * s)  , S) -- 6 -> 2 (Redundant with above?) No, this is x == 4S, different edge.
    | ny == -1    && nx <  3 * s = (P (3*s - 1 - nx) (ny + s + 1)  , E) -- 4 -> 1 (y=-1, x < 3s)
    | ny == -1    && nx >= 3 * s = (P (ny + 1)       (nx - 2 * s)  , S) -- 6 -> 1 (y=-1, x >= 3s)
    | ny == s - 1 && nx < s      = (P (3*s - 1 - nx) (ny - s + 1)  , E) -- 1 -> 4
    | ny == s - 1 && nx >= s && dir == W = (P (ny + s + 1) (nx - s)    , S) -- 3 -> 1
    | ny == s     && dir == E    = (P (ny + 2*s - 1) (nx - 2 * s)  , N) -- 2 -> 3
    | ny == 2 * s && nx < 2 * s && dir == E = (P (ny - s - 1) (nx + s)    , N) -- 5 -> 2
    | ny == 2*s   && nx >= 2 * s = (P (3*s - 1 - nx) (ny + s - 1)  , W) -- 4 -> 5
    | ny == 3 * s               = (P (3*s - 1 - nx) (ny - s - 1)  , W) -- 2 -> 5
    | otherwise                 = error ("Not a border crossing: " ++ show (P nx ny) ++ " " ++ show dir)
    where s = size

parsePath :: String -> [Movement]
parsePath [] = []
parsePath s@(c:cs)
    | c == 'R' || c == 'L' = Rotate c : parsePath cs
    | isDigit c = let (digits, rest) = span isDigit s
                  in Steps (read digits) : parsePath rest
    | otherwise = error "Invalid path character"

parseInput :: String -> (Map P Bool, Int, [Movement])
parseInput contents = (mapData, size, movements)
  where
    ls = lines contents
    (mapLines, pathLines) = break null ls
    rawPath = case pathLines of (_:p:_) -> p; _ -> ""
    
    size = length (head mapLines) `div` 3 -- Assumes structure where first line spans 3 faces

    mapData = M.fromList [ (P r c, char == '#')
                         | (r, line) <- zip [0..] mapLines
                         , (c, char) <- zip [0..] line
                         , char /= ' '
                         ]
    movements = parsePath rawPath

walk :: Map P Bool -> Int -> Human -> (P, Dir)
walk mapData size human =
  let P cx cy = curr human
      f       = facing human
      P dx dy = dirDelta f
      nextPos = P (cx + dx) (cy + dy)
  in case M.lookup nextPos mapData of
       Just True  -> (curr human, f) -- Hit wall
       Just False -> (nextPos, f)    -- Open space
       Nothing    ->                 -- Off map, cross border
         let (newPos, newFacing) = crossBorder nextPos f size
         in if fromMaybe False (M.lookup newPos mapData) -- Check for wall after wrap
            then (curr human, f)      -- Hit wall after wrap
            else (newPos, newFacing) -- Wrapped successfully

processMovement :: Map P Bool -> Int -> Human -> Movement -> Human
processMovement mapData size human (Rotate c) = human { facing = rotate (facing human) c }
processMovement mapData size human (Steps n)  = walkN n human
  where
    walkN 0 h = h
    walkN k h =
        let (nextP, nextF) = walk mapData size h
        in if curr h == nextP && facing h == nextF
           then h -- Stuck
           else walkN (k-1) (Human nextP nextF)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (mapData, size, movements) = parseInput contents
      
      -- Python starts at P(0, size), assuming the layout starts at (0, size).
      -- Let's find the actual minimum column in row 0 that is traversable.
      startCol = minimum [ c | p@(P r c) <- M.keys mapData, r == 0, not (mapData M.! p) ]
      initialHuman = Human (P 0 startCol) E -- Start at row 0, min valid col, facing East

      finalHuman = foldl' (processMovement mapData size) initialHuman movements
      finalValue = 1000 * (x (curr finalHuman) + 1) + 4 * (y (curr finalHuman) + 1) + points (facing finalHuman)
  print finalValue

