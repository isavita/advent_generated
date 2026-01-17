
import qualified Data.Map.Strict as M
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as S
import Data.List (foldl')

data PulseValue = Low | High deriving (Eq, Show)

data ModuleType = FlipFlop | Conjunction | Broadcaster deriving (Eq, Show)

data Module = Module
  { mType :: ModuleType
  , dests :: [String]
  , ffState :: Bool
  , conjMem :: M.Map String PulseValue
  } deriving (Show)

splitOn :: String -> String -> [String]
splitOn delim str = case breakList (== delim) str of
  (a, []) -> [a]
  (a, _:rest) -> a : splitOn delim (drop (length delim - 1) rest)
  where
    breakList _ [] = ([], [])
    breakList p s@(x:xs)
      | take (length delim) s == delim = ([], s)
      | otherwise = let (ys, zs) = breakList p xs in (x:ys, zs)

parseInput :: String -> M.Map String Module
parseInput input =
  let lines' = lines input
      rawModules = map parseLine lines'
      parseLine line =
        let [namePart, destPart] = splitOn " -> " line
            ds = splitOn ", " destPart
            (mtype, name) = case head namePart of
              '%' -> (FlipFlop, tail namePart)
              '&' -> (Conjunction, tail namePart)
              _   -> (Broadcaster, namePart)
        in (name, Module mtype ds False M.empty)
      initialMap = M.fromList rawModules
  in M.mapWithKey (\name mod ->
      if mType mod == Conjunction
      then mod { conjMem = M.fromList [(n, Low) | (n, m) <- M.toList initialMap, name `elem` dests m] }
      else mod) initialMap

processPulses :: Seq (String, String, PulseValue) -> M.Map String Module -> (Int, Int) -> (M.Map String Module, (Int, Int))
processPulses Empty mods counts = (mods, counts)
processPulses ((src, target, val) :<| rest) mods (low, high) =
  let (nLow, nHigh) = if val == Low then (low + 1, high) else (low, high + 1)
  in case M.lookup target mods of
    Nothing -> processPulses rest mods (nLow, nHigh)
    Just m -> case mType m of
      Broadcaster ->
        let nextPulses = S.fromList [(target, d, val) | d <- dests m]
        in processPulses (rest <> nextPulses) mods (nLow, nHigh)
      FlipFlop ->
        if val == High 
        then processPulses rest mods (nLow, nHigh)
        else
          let newState = not (ffState m)
              newVal = if newState then High else Low
              newM = m { ffState = newState }
              nextPulses = S.fromList [(target, d, newVal) | d <- dests m]
          in processPulses (rest <> nextPulses) (M.insert target newM mods) (nLow, nHigh)
      Conjunction ->
        let newMem = M.insert src val (conjMem m)
            newVal = if all (== High) (M.elems newMem) then Low else High
            newM = m { conjMem = newMem }
            nextPulses = S.fromList [(target, d, newVal) | d <- dests m]
          in processPulses (rest <> nextPulses) (M.insert target newM mods) (nLow, nHigh)

solve :: String -> Int
solve input =
  let initialModules = parseInput input
      runStep (m, (l, h)) _ =
        let (nm, (nl, nh)) = processPulses (S.singleton ("button", "broadcaster", Low)) m (0, 0)
        in (nm, (l + nl, h + nh))
      (_, (totalLow, totalHigh)) = foldl' runStep (initialModules, (0, 0)) [1..1000]
  in totalLow * totalHigh

main :: IO ()
main = do
  content <- readFile "input.txt"
  print $ solve content
