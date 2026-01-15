
import System.IO
import Data.List (sort, foldl', maximumBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

type Minute = Int
type Guard  = Int

step :: (Maybe Guard, Maybe Minute, [(Guard, Minute)]) -> String
     -> (Maybe Guard, Maybe Minute, [(Guard, Minute)])
step (mg, ms, acc) line =
    case words line of
        ws | "Guard" `elem` ws ->
                (Just (read (tail (ws !! 3))), Nothing, acc)
           | "falls" `elem` ws ->
                let m = read (take 2 (drop 3 (ws !! 1))) in (mg, Just m, acc)
           | "wakes" `elem` ws ->
                let wake = read (take 2 (drop 3 (ws !! 1)))
                    guard = fromJust mg
                    start = fromJust ms
                    new = [(guard, m) | m <- [start .. wake - 1]]
                in (mg, Nothing, acc ++ new)
           | otherwise -> (mg, ms, acc)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let sortedLines = sort (lines content)
        (_, _, mins) = foldl' step (Nothing, Nothing, []) sortedLines
        guardMap = foldl' (\m (g,mi) -> M.insertWith (M.unionWith (+)) g (M.singleton mi 1) m)
                          M.empty mins

        (sleepiestGuard, minuteMap) =
            maximumBy (comparing (sum . M.elems . snd)) (M.toList guardMap)

        mostCommonMinute = fst $ maximumBy (comparing snd) (M.toList minuteMap)
        result1 = sleepiestGuard * mostCommonMinute

        (guard2, minute2, _) =
            maximumBy (comparing (\(_,_,c) -> c))
                [ (g,m,c) | (g,im) <- M.toList guardMap, (m,c) <- M.toList im ]

        result2 = guard2 * minute2

    print result1
    print result2
