
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, listToMaybe)
import Data.List (foldl')
import System.IO (readFile)

-- Type Definitions
type Pos = (Int, Int) -- (Row, Column)
type ElfSet = Set.Set Pos
data Direction = N | S | W | E deriving (Eq, Show, Enum, Bounded)

-- Initial direction order
initialDirections :: [Direction]
initialDirections = [N, S, W, E]

-- Calculate the 8 neighbors of a position
neighbors :: Pos -> [Pos]
neighbors (r, c) = [(r + dr, c + dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1], (dr, dc) /= (0, 0)]

-- Calculate the next position when moving in a given direction
nextPosition :: Direction -> Pos -> Pos
nextPosition N (r, c) = (r - 1, c)
nextPosition S (r, c) = (r + 1, c)
nextPosition W (r, c) = (r, c - 1)
nextPosition E (r, c) = (r, c + 1)

-- Positions to check for vacancy before proposing a move in a given direction
positionsToCheck :: Direction -> Pos -> [Pos]
positionsToCheck N (r, c) = [(r - 1, c - 1), (r - 1, c), (r - 1, c + 1)]
positionsToCheck S (r, c) = [(r + 1, c - 1), (r + 1, c), (r + 1, c + 1)]
positionsToCheck W (r, c) = [(r - 1, c - 1), (r, c - 1), (r + 1, c - 1)]
positionsToCheck E (r, c) = [(r - 1, c + 1), (r, c + 1), (r + 1, c + 1)]

-- Rotate the direction list
rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

-- Parse the input grid into a Set of Elf positions
parseInput :: String -> ElfSet
parseInput input = Set.fromList
    [ (r, c)
    | (r, row) <- zip [0..] (lines input)
    , (c, char) <- zip [0..] row
    , char == '#'
    ]

-- First half of the round: Elves propose moves
proposeMoves :: ElfSet -> [Direction] -> Map.Map Pos Pos
proposeMoves elves dirOrder = Map.fromList $ map (proposeOne elves dirOrder) (Set.toList elves)
  where
    proposeOne :: ElfSet -> [Direction] -> Pos -> (Pos, Pos) -- (CurrentPos, ProposedPos)
    proposeOne currentElves directions currentPos =
        -- Check if any neighbors are occupied
        let eightNeighbors = neighbors currentPos
            hasOccupiedNeighbor = any (`Set.member` currentElves) eightNeighbors
        in if not hasOccupiedNeighbor
           then (currentPos, currentPos) -- No neighbors, propose staying
           else case findValidProposal currentElves directions currentPos of
                  Just proposedPos -> (currentPos, proposedPos)
                  Nothing          -> (currentPos, currentPos) -- No valid move found, propose staying

    findValidProposal :: ElfSet -> [Direction] -> Pos -> Maybe Pos -- Just ProposedPos or Nothing
    findValidProposal currentElves directions currentPos =
        listToMaybe $ mapMaybe (checkAndGetNext currentElves currentPos) directions

    checkAndGetNext :: ElfSet -> Pos -> Direction -> Maybe Pos
    checkAndGetNext currentElves pos dir =
        let checkPos = positionsToCheck dir pos
            nextPos  = nextPosition dir pos
        -- Check if all three positions to check are empty
        in if all (\p -> not $ Set.member p currentElves) checkPos
           then Just nextPos
           else Nothing

-- Second half of the round: Elves move based on proposals and conflicts
applyMoves :: ElfSet -> Map.Map Pos Pos -> ElfSet
applyMoves currentElves proposals =
    let proposalList = Map.toList proposals -- [(CurrentPos, ProposedPos)]
        -- Count how many elves proposed each destination
        destCounts = Map.fromListWith (+) [(proposed, 1 :: Int) | (_, proposed) <- proposalList]

        -- Determine the final position for each elf based on its proposal and conflicts
        resolve :: (Pos, Pos) -> Pos -- (CurrentPos, ProposedPos) -> FinalPos
        resolve (current, proposed) =
            -- If the destination was proposed by only one elf, move there. Otherwise, stay put.
            if Map.findWithDefault 0 proposed destCounts == 1
            then proposed
            else current

    in Set.fromList $ map resolve proposalList

-- Perform one full round of simulation
runRound :: (ElfSet, [Direction]) -> (ElfSet, [Direction])
runRound (elves, dirs) =
    let proposals = proposeMoves elves dirs
        nextElves = applyMoves elves proposals
        nextDirs  = rotate dirs
    in (nextElves, nextDirs)

-- Simulate the process for a given number of rounds
simulate :: Int -> ElfSet -> [Direction] -> ElfSet
simulate rounds initialElves initialDirs =
    fst $ foldl' (\state _ -> runRound state) (initialElves, initialDirs) [1..rounds]

-- Calculate the number of empty tiles in the bounding box
calculateEmptyTiles :: ElfSet -> Int
calculateEmptyTiles elves
    | Set.null elves = 0
    | otherwise =
        let (rows, cols) = unzip $ Set.toList elves
            minRow = minimum rows
            maxRow = maximum rows
            minCol = minimum cols
            maxCol = maximum cols
            width  = maxCol - minCol + 1
            height = maxRow - minRow + 1
            area   = width * height
            elfCount = Set.size elves
        in area - elfCount

-- Main function
main :: IO ()
main = do
    input <- readFile "input.txt"
    let initialElves = parseInput input
    
    -- Simulate 10 rounds
    let finalElves = simulate 10 initialElves initialDirections
    
    -- Calculate and print the result
    let emptyTiles = calculateEmptyTiles finalElves
    print emptyTiles
