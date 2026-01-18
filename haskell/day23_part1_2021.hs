
import qualified Data.Set as S
import Data.Maybe (fromJust)
import System.IO (readFile)

type State = (String, [String])

energyCost :: Char -> Int
energyCost 'A' = 1
energyCost 'B' = 10
energyCost 'C' = 100
energyCost 'D' = 1000

targetCol :: Int -> Int
targetCol r = 2 + 2 * r

charToRoom :: Char -> Int
charToRoom 'A' = 0
charToRoom 'B' = 1
charToRoom 'C' = 2
charToRoom 'D' = 3
charToRoom _   = error "Invalid"

isPathClear :: Int -> Int -> String -> Bool
isPathClear h1 h2 hallway =
    let (start, end) = (min h1 h2, max h1 h2)
        pathIndices = [i | i <- [start..end], i /= h1]
    in all (\i -> hallway !! i == '.') pathIndices

isRoomClean :: Int -> [String] -> Bool
isRoomClean rIdx rooms =
    let target = "ABCD" !! rIdx
    in all (\c -> c == '.' || c == target) (rooms !! rIdx)

deepestPos :: String -> Int
deepestPos room = fst . last . filter (\(_, c) -> c == '.') $ zip [0..] room

topmostAmphipod :: String -> Maybe (Int, Char)
topmostAmphipod room = case filter (\(_, c) -> c /= '.') (zip [0..] room) of
    [] -> Nothing
    (x:_) -> Just x

isRoomComplete :: Int -> [String] -> Bool
isRoomComplete rIdx rooms =
    let target = "ABCD" !! rIdx
    in all (== target) (rooms !! rIdx)

replace :: Int -> a -> [a] -> [a]
replace i val lst = take i lst ++ [val] ++ drop (i + 1) lst

solve :: State -> Int
solve startNode = dijkstra (S.singleton (0, startNode)) S.empty
  where
    isGoal (_, rms) = all (\i -> isRoomComplete i rms) [0..3]
    dijkstra pq visited
        | S.null pq = -1
        | S.member state visited = dijkstra rest visited
        | isGoal state = cost
        | otherwise = dijkstra (foldr (\(c, s) q -> S.insert (cost + c, s) q) rest (nextMoves state)) (S.insert state visited)
      where ((cost, state), rest) = S.deleteFindMin pq

    nextMoves (hallway, rooms) = roomToHallway ++ hallwayToRoom
      where
        roomToHallway = do
            rIdx <- [0..3]
            let targetChar = "ABCD" !! rIdx
                room = rooms !! rIdx
            if all (\c -> c == '.' || c == targetChar) room then []
            else case topmostAmphipod room of
                Nothing -> []
                Just (d, char) -> do
                    hIdx <- [0, 1, 3, 5, 7, 9, 10]
                    if isPathClear (targetCol rIdx) hIdx hallway
                    then let moveCost = (d + 1 + abs (hIdx - targetCol rIdx)) * energyCost char
                             newHallway = replace hIdx char hallway
                             newRooms = replace rIdx (replace d '.' room) rooms
                         in [(moveCost, (newHallway, newRooms))]
                    else []
        hallwayToRoom = do
            (hIdx, char) <- zip [0..] hallway
            if char == '.' then []
            else let rIdx = charToRoom char in
                if isRoomClean rIdx rooms && isPathClear hIdx (targetCol rIdx) hallway
                then let d = deepestPos (rooms !! rIdx)
                         moveCost = (d + 1 + abs (hIdx - targetCol rIdx)) * energyCost char
                         newHallway = replace hIdx '.' hallway
                         newRooms = replace rIdx (replace d char (rooms !! rIdx)) rooms
                     in [(moveCost, (newHallway, newRooms))]
                else []

main :: IO ()
main = do
    content <- readFile "input.txt"
    let ls = lines content
        roomCols = [3, 5, 7, 9]
        initialRooms = [[(ls !! r) !! c | r <- [2 .. (length ls - 2)]] | c <- roomCols]
    print $ solve ("...........", initialRooms)
