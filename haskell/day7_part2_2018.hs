
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Data.Char       (ord)
import           Data.List       (foldl', sort, nub)
import           Data.Maybe      (fromMaybe)

type Task = Char
type Dependencies = M.Map Task [Task]
type InDegrees = M.Map Task Int
type Workers = M.Map Task Int -- Task -> Remaining Time
type State = (Int, InDegrees, [Task], Workers) -- Time, InDegrees, Available, Workers

workerCount :: Int
workerCount = 5

baseTime :: Int
baseTime = 60

taskDuration :: Task -> Int
taskDuration task = baseTime + ord task - ord 'A' + 1

parseLine :: String -> Maybe (Task, Task)
parseLine line = case words line of
    ("Step":p:"must":"be":"finished":"before":"step":s:"can":"begin.":_) -> Just (head p, head s)
    _ -> Nothing

parseInput :: String -> (Dependencies, InDegrees)
parseInput content = (deps, finalInDegrees)
  where
    pairs = mapMaybe parseLine (lines content)
    allTasks = S.fromList $ concatMap (\(p, s) -> [p, s]) pairs
    
    deps = foldl' buildDeps M.empty pairs
    buildDeps m (p, s) = M.insertWith (++) p [s] m

    initialInDegrees = M.fromSet (\_ -> 0) allTasks
    incrementInDegrees m (_, s) = M.adjust (+1) s m
    finalInDegrees = foldl' incrementInDegrees initialInDegrees pairs

    mapMaybe f = foldr (\x acc -> case f x of Just y -> y : acc; Nothing -> acc) []

simulate :: Dependencies -> State -> Int
simulate deps (time, degrees, available, workers)
    | null available && M.null workers = time
    | otherwise = simulate deps nextState
  where
    -- Assign tasks to free workers
    freeWorkers = workerCount - M.size workers
    tasksToAssign = take freeWorkers available
    newAvailable = drop freeWorkers available
    newWorkers = foldl' (\w t -> M.insert t (taskDuration t) w) workers tasksToAssign

    -- If no workers are busy, time doesn't advance yet, just loop to assign if possible
    (timeDelta, updatedWorkers)
      | M.null newWorkers = (0, newWorkers)
      | otherwise =
          let minTime = minimum (M.elems newWorkers)
          in (minTime, M.map (\t -> t - minTime) newWorkers)

    -- Find completed tasks
    completedTasks = M.keys $ M.filter (<= 0) updatedWorkers
    remainingWorkers = M.filter (> 0) updatedWorkers

    -- Update degrees based on completed tasks
    (updatedDegrees, newlyAvailable) = foldl' processCompletion (degrees, []) completedTasks

    processCompletion :: (InDegrees, [Task]) -> Task -> (InDegrees, [Task])
    processCompletion (currentDegrees, newAvail) completedTask =
        let successors = fromMaybe [] (M.lookup completedTask deps)
        in foldl' (updateSuccessorDegree completedTask) (currentDegrees, newAvail) successors

    updateSuccessorDegree :: Task -> (InDegrees, [Task]) -> Task -> (InDegrees, [Task])
    updateSuccessorDegree _ (deg, avail) successor =
        let newDegMap = M.adjust (\d -> d - 1) successor deg
            newDegree = newDegMap M.! successor
        in if newDegree == 0
           then (newDegMap, successor : avail)
           else (newDegMap, avail)

    -- Combine and sort new available tasks
    finalAvailable = sort . nub $ newAvailable ++ newlyAvailable

    nextState = (time + timeDelta, updatedDegrees, finalAvailable, remainingWorkers)


main :: IO ()
main = do
    content <- readFile "input.txt"
    let (dependencies, initialDegrees) = parseInput content
    let initialAvailable = sort [task | (task, degree) <- M.toList initialDegrees, degree == 0]
    let initialState = (0, initialDegrees, initialAvailable, M.empty)
    let totalTime = simulate dependencies initialState
    print totalTime

