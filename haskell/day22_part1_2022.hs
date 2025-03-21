
import Data.Char (isDigit, isSpace)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import System.IO

data Direction = R | D | L | U deriving (Show, Eq, Enum)

type Position = (Int, Int)

type Grid = M.Map Position Char

type Boundaries = M.Map Int (Int, Int)

parseMap :: [String] -> (Grid, Boundaries, Boundaries, Int, Int)
parseMap lines = (grid, rowBoundaries, colBoundaries, numRows, numCols)
  where
    numRows = length lines
    numCols = maximum (map length lines)
    gridList =
      [ ((x, y), c)
        | (y, row) <- zip [1 ..] lines,
          (x, c) <- zip [1 ..] row,
          c /= ' '
      ]
    grid = M.fromList gridList
    rowBoundaries =
      M.fromList
        [ (y, (minimum xs, maximum xs))
          | (y, row) <- zip [1 ..] lines,
            let xs = [x | (x, c) <- zip [1 ..] row, c /= ' '],
            not (null xs)
        ]
    colBoundaries =
      M.fromListWith
        (\(min1, max1) (min2, max2) -> (min min1 min2, max max1 max2))
        [ (x, (y, y))
          | ((x, y), c) <- gridList
        ]

parsePath :: String -> [Either Int Char]
parsePath "" = []
parsePath s = case span isDigit s of
  (numStr, rest) ->
    case reads numStr :: [(Int, String)] of
      [(num, _)] -> Left num : parseRest rest
      _ -> parseRest rest
  where
    parseRest "" = []
    parseRest (c : rest) = Right c : parsePath rest

findStartingPosition :: Grid -> Position
findStartingPosition grid = head [(x, 1) | x <- [1 ..], M.lookup (x, 1) grid == Just '.']

turnDirection :: Direction -> Char -> Direction
turnDirection dir turn = case (dir, turn) of
  (R, 'R') -> D
  (R, 'L') -> U
  (D, 'R') -> L
  (D, 'L') -> R
  (L, 'R') -> U
  (L, 'L') -> D
  (U, 'R') -> R
  (U, 'L') -> L
  _ -> error "Invalid turn instruction."

move :: Direction -> Position -> Position
move R (x, y) = (x + 1, y)
move L (x, y) = (x - 1, y)
move U (x, y) = (x, y - 1)
move D (x, y) = (x, y + 1)

wrap :: Grid -> Boundaries -> Boundaries -> Int -> Int -> Direction -> Position -> Position
wrap grid rowBoundaries colBoundaries numRows numCols dir (x, y) = case dir of
  R ->
    if x > snd (rowBoundaries M.! y)
      then (fst (rowBoundaries M.! y), y)
      else (x, y)
  L ->
    if x < fst (rowBoundaries M.! y)
      then (snd (rowBoundaries M.! y), y)
      else (x, y)
  U ->
    if y < fst (colBoundaries M.! x)
      then (x, snd (colBoundaries M.! x))
      else (x, y)
  D ->
    if y > snd (colBoundaries M.! x)
      then (x, fst (colBoundaries M.! x))
      else (x, y)

simulate :: Grid -> Boundaries -> Boundaries -> Int -> Int -> [Either Int Char] -> (Position, Direction)
simulate grid rowBoundaries colBoundaries numRows numCols instructions =
  foldl' step (startingPosition, R) instructions
  where
    startingPosition = findStartingPosition grid
    step :: (Position, Direction) -> Either Int Char -> (Position, Direction)
    step (pos, dir) (Left steps) = moveSteps pos dir steps
    step (pos, dir) (Right turn) = (pos, turnDirection dir turn)
    moveSteps :: Position -> Direction -> Int -> (Position, Direction)
    moveSteps pos dir 0 = (pos, dir)
    moveSteps pos dir steps =
      let nextPos' = move dir pos
          nextPos = wrap grid rowBoundaries colBoundaries numRows numCols dir nextPos'
       in case M.lookup nextPos grid of
            Just '#' -> (pos, dir)
            Just '.' -> moveSteps nextPos dir (steps - 1)
            Nothing -> (pos, dir) -- Should not happen
            _ -> (pos, dir)

computePassword :: (Position, Direction) -> Int
computePassword ((x, y), facing) = 1000 * y + 4 * x + facingValue
  where
    facingValue = case facing of
      R -> 0
      D -> 1
      L -> 2
      U -> 3

readInput :: FilePath -> IO ([String], String)
readInput filename = do
  contents <- readFile filename
  let ls = lines contents
      (mapLines, rest) = break (=="") ls
  return (mapLines, unlines $ tail rest)


main :: IO ()
main = do
  (mapLines, pathStr) <- readInput "input.txt"
  let (grid, rowBoundaries, colBoundaries, numRows, numCols) = parseMap mapLines
      instructions = parsePath (filter (not . isSpace) pathStr)
      (finalPosition, finalFacing) = simulate grid rowBoundaries colBoundaries numRows numCols instructions
      password = computePassword (finalPosition, finalFacing)
  print password
