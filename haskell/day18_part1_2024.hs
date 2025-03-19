
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.Foldable (toList)

type Point = (Int, Int)
type Grid = S.Set Point
type Queue = Q.Seq (Int, Int, Int)

gridSize :: Int
gridSize = 71

parseInput :: String -> Grid
parseInput = S.fromList . map parseLine . take 1024. lines
  where
    parseLine line = let [x, y] = map read $ wordsWhen (==',') line in (x, y)
    wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
main :: IO ()
main = do
  contents <- readFile "input.txt"
  let corrupted = parseInput contents
  let start = (0, 0, 0) :: (Int, Int, Int)
  let visited = S.singleton (0, 0) :: S.Set (Int, Int)
  let queue = Q.singleton start :: Queue
  print $ solve gridSize corrupted queue visited

solve :: Int -> Grid -> Queue -> S.Set Point -> Int
solve size corrupted queue visited =
  case Q.viewl queue of
    Q.EmptyL -> -1
    (x, y, steps) Q.:< rest ->
      if x == size - 1 && y == size - 1
      then steps
      else
        let neighbors = [(x + dx, y + dy) | (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)]]
            validNeighbors = filter (\(nx, ny) -> 0 <= nx && nx < size && 0 <= ny && ny < size &&
                                        not (S.member (nx, ny) corrupted) && not (S.member (nx, ny) visited)) neighbors
            newQueue = foldl (\q (nx, ny) -> q Q.|> (nx, ny, steps + 1)) rest validNeighbors
            newVisited = foldl (\s (nx,ny) -> S.insert (nx,ny) s) visited validNeighbors
        in solve size corrupted newQueue newVisited
