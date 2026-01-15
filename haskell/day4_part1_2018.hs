
import System.IO
import Data.List (sort, maximumBy, isInfixOf)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

getMinute :: String -> Int
getMinute s = read (take 2 (drop 1 (dropWhile (/= ':') s))) :: Int

process :: [String]
        -> Maybe Int               -- current guard
        -> Maybe Int               -- sleep start minute
        -> M.Map Int Int           -- total minutes per guard
        -> M.Map Int (M.Map Int Int) -- minutes count per guard
        -> (M.Map Int Int, M.Map Int (M.Map Int Int))
process [] _ _ tot mins = (tot, mins)
process (l:ls) curGuard curSleep tot mins
  | "Guard" `isInfixOf` l =
      let g = read (takeWhile (/= ' ') (drop 1 (dropWhile (/= '#') l))) :: Int
      in process ls (Just g) Nothing tot mins
  | "falls asleep" `isInfixOf` l = process ls curGuard (Just (getMinute l)) tot mins
  | "wakes up" `isInfixOf` l =
      let wake = getMinute l
          Just g = curGuard
          Just sleep = curSleep
          rng = [sleep .. wake-1]
          tot' = foldr (\_ -> M.insertWith (+) g 1) tot rng
          mins' = foldr (\m -> M.insertWith (M.unionWith (+)) g (M.singleton m 1)) mins rng
      in process ls curGuard Nothing tot' mins'
  | otherwise = process ls curGuard curSleep tot mins

main :: IO ()
main = do
  txt <- readFile "input.txt"
  let (total, minutes) = process (sort . lines $ txt) Nothing Nothing M.empty M.empty
      (sleepiestGuard, _) = maximumBy (comparing snd) (M.toList total)
      minuteMap = fromJust (M.lookup sleepiestGuard minutes)
      (sleepiestMinute, _) = maximumBy (comparing snd) (M.toList minuteMap)
  print (sleepiestGuard * sleepiestMinute)
