
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import Data.Map (Map, empty, insert, lookup, (!))
import qualified Data.Map as Map
import Control.Monad.State

type Item = Int
type Floor = Int
type State = (Floor, Map Item Floor)
type Visited = Set State
type Queue = [(State, Int)]

-- Parse input
parseInput :: String -> State
parseInput input = (1, Map.fromList $ concatMap parseLine (zip [1..] (lines input)))
  where
    parseLine :: (Floor, String) -> [(Item, Floor)]
    parseLine (floor, line) = concatMap (parseItem floor) (words line)
    parseItem :: Floor -> String -> [(Item, Floor)]
    parseItem floor word
      | "generator" `isSuffixOf` word = [(read (takeWhile isDigit word), floor)]
      | "microchip" `isSuffixOf` word = [(read (takeWhile isDigit word) * (-1), floor)]
      | otherwise = []
    isSuffixOf :: String -> String -> Bool
    isSuffixOf suffix str = reverse suffix `isPrefixOf` reverse str
    isDigit :: Char -> Bool
    isDigit c = c >= '0' && c <= '9'

-- Check if a state is valid
isValid :: State -> Bool
isValid (_, items) = all isValidFloor [1..4]
  where
    isValidFloor :: Floor -> Bool
    isValidFloor floor = not $ any fried $ filter ((== floor) . snd) $ Map.toList items
      where
        fried :: (Item, Floor) -> Bool
        fried (chip, _) = chip < 0 && any ((> 0) . fst) (filter ((== floor) . snd) $ Map.toList items) && not (Map.member (chip * (-1)) items && items ! (chip * (-1)) == floor)

-- Generate possible next states
nextStates :: State -> [State]
nextStates (elevatorFloor, items) = concatMap (moveItems elevatorFloor items) possibleMoves
  where
    possibleMoves = filter (isValidMove elevatorFloor items) $ concatMap (\n -> map (\x -> (n, x)) (subsets $ filter ((== elevatorFloor) . snd) $ Map.toList items)) [-1, 1]
    isValidMove :: Floor -> Map Item Floor -> (Int, [ (Item, Floor)]) -> Bool
    isValidMove _ _ (_, []) = False
    isValidMove floor items (direction, itemsToMove) =
      let nextFloor = floor + direction
      in nextFloor >= 1 && nextFloor <= 4 && length itemsToMove <= 2
    moveItems :: Floor -> Map Item Floor -> (Int, [(Item, Floor)]) -> [State]
    moveItems floor items (direction, itemsToMove) =
      let nextFloor = floor + direction
          newItems = foldl (\acc (item, _) -> Map.insert item nextFloor acc) items itemsToMove
      in [(nextFloor, newItems)]
    subsets :: [a] -> [[a]]
    subsets [] = [[]]
    subsets (x:xs) = let rest = subsets xs in map (x:) rest ++ rest

-- Check if a state is the goal state
isGoal :: State -> Bool
isGoal (_, items) = all ((== 4) . snd) $ Map.toList items

-- Breadth-first search
bfs :: State -> Int
bfs initialState = bfs' empty [(initialState, 0)]
  where
    bfs' :: Visited -> Queue -> Int
    bfs' visited [] = error "No solution found"
    bfs' visited ((state, steps):queue)
      | state `member` visited = bfs' visited queue
      | isGoal state = steps
      | otherwise = bfs' (insert state visited) (queue ++ map (\nextState -> (nextState, steps + 1)) (filter (`notMember` visited) $ nextStates state))
    notMember :: State -> Set State -> Bool
    notMember = not . member

main :: IO ()
main = do
  input <- readFile "input.txt"
  let initialState = parseInput input
  let result = bfs initialState
  print result
