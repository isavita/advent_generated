
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (maximumBy)
import Data.Ord (comparing)

type Point = (Int, Int)
type DistMap = M.Map Point (M.Map Point Bool)
type Visited = M.Map Point Int

move :: Point -> Char -> Point
move (x, y) dir
  | dir == 'N' = (x, y - 1)
  | dir == 'S' = (x, y + 1)
  | dir == 'E' = (x + 1, y)
  | dir == 'W' = (x - 1, y)
  | otherwise = (x, y)

buildMap :: String -> DistMap
buildMap regex = go regex (0, 0) [] M.empty
  where
    go [] _ _ dm = dm
    go (c:cs) cp stack dm
      | c == '(' = go cs cp (cp : stack) dm
      | c == '|' = go cs (head stack) stack dm
      | c == ')' = go cs (head stack) (tail stack) dm
      | otherwise =
        let np = move cp c
            dm' = M.insertWith M.union cp (M.singleton np True) dm
        in go cs np stack dm'

findFurthestRoom :: DistMap -> Int
findFurthestRoom dm = maximum . map snd . M.toList $ bfs (S.singleton (0,0)) [(0, 0)] (M.singleton (0,0) 0)
    where
        bfs visited queue distances = case queue of
            [] -> distances
            (p:ps) ->
                let neighbors = M.keys $ M.findWithDefault M.empty p dm
                    newNeighbors = filter (\x -> not (S.member x visited)) neighbors
                    newVisited = S.union visited (S.fromList newNeighbors)
                    newQueue = ps ++ newNeighbors
                    newDistances = foldl (\acc np -> M.insert np (distances M.! p + 1) acc) distances newNeighbors
                in bfs newVisited newQueue newDistances

main :: IO ()
main = do
  regex <- readFile "input.txt"
  let dm = buildMap $ init . tail $ filter (/='\n') regex
  print $ findFurthestRoom dm
