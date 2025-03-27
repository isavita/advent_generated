
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.List ((\\))

type Pos = (Int, Int)

addPos :: Pos -> Pos -> Pos
addPos (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

-- Python's dirs order: NW, N, NE, E, SE, S, SW, W
pyDirs :: [Pos]
pyDirs = [(-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1)]

-- Python's order: N, S, W, E (indices into pyDirs) -> 1, 5, 7, 3
pyOrderIndices :: [Int]
pyOrderIndices = [1, 5, 7, 3]

-- Get the 3 directions to check for a given main direction index (from pyDirs)
getCheckDirs :: Int -> [Pos]
getCheckDirs dirIdx = [ pyDirs !! ((dirIdx + di - 1 + 8) `mod` 8) | di <- [0, 1, 2] ]

parse :: String -> S.Set Pos
parse = S.fromList . concat . zipWith parseLine [0..] . lines
  where
    parseLine r line = [(r, c) | (c, char) <- zip [0..] line, char == '#']

runRound :: Int -> S.Set Pos -> (S.Set Pos, Bool)
runRound dirOffset elves =
    let
        allNeighbors p = [ addPos p d | d <- pyDirs ]
        isOccupied pos = S.member pos elves

        propose :: Pos -> Maybe (Pos, Pos) -- (OriginalPos, ProposedDest)
        propose p
            | all (not . isOccupied) (allNeighbors p) = Nothing -- No move if isolated
            | otherwise = findProposal p 0
          where
            findProposal :: Pos -> Int -> Maybe (Pos, Pos)
            findProposal currentPos k
                | k >= 4 = Nothing -- No valid direction found
                | otherwise =
                    let dirIdx = pyOrderIndices !! ((dirOffset + k) `mod` 4)
                        checks = getCheckDirs dirIdx
                        moveDir = pyDirs !! dirIdx
                        dest = addPos currentPos moveDir
                    in if any (\checkDir -> isOccupied (addPos currentPos checkDir)) checks
                       then findProposal currentPos (k + 1) -- Try next direction
                       else Just (currentPos, dest) -- Found valid proposal

        proposals = mapMaybe propose (S.toList elves) -- List of (elf, dest)

        destCounts = M.fromListWith (+) [(dest, 1) | (_, dest) <- proposals]

        validMoves = [(p, dest) | (p, dest) <- proposals, M.lookup dest destCounts == Just 1]

        movedElvesOrigins = map fst validMoves
        newPositions = map snd validMoves

        -- Elves that didn't move (either isolated, blocked proposal, or collision)
        stationaryElves = S.toList elves \\ movedElvesOrigins

        newElvesSet = S.fromList (stationaryElves ++ newPositions)

        -- Check if any elf successfully moved to a new unique position
        moved = not $ null validMoves

    in (newElvesSet, moved)

simulate :: Int -> S.Set Pos -> Int
simulate roundNum elves =
    let dirOffset = roundNum `mod` 4
        (newElves, moved) = runRound dirOffset elves
    in if moved
       then simulate (roundNum + 1) newElves
       else roundNum + 1 -- Return the first round number where no movement happened

main :: IO ()
main = do
    input <- readFile "input.txt"
    let initialElves = parse input
    let finalRound = simulate 0 initialElves
    print finalRound

