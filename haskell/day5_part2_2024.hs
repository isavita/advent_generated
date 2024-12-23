
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (forM_)
import System.IO

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (rules, updates) = parseInput contents
  let total = sum $ map (processUpdate rules) updates
  print total

parseInput :: String -> ([([Int], [Int])], [[Int]])
parseInput contents =
  let ls = lines contents
      (ruleLines, updateLines) = break (== "") ls
      rules = map parseRule $ filter (not . null) ruleLines
      updates = map parseUpdate $ filter (not . null) $ tail updateLines
  in (rules, updates)

parseRule :: String -> ([Int], [Int])
parseRule line =
  let [x, y] = map (read . trim) $ splitOn "|" line
  in ([x], [y])

parseUpdate :: String -> [Int]
parseUpdate line = map (read . trim) $ splitOn "," line

splitOn :: String -> String -> [String]
splitOn delim str = case break (== head delim) str of
    (pre, "") -> [pre]
    (pre, suf) -> pre : splitOn delim (tail suf)

trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

processUpdate :: [([Int], [Int])] -> [Int] -> Int
processUpdate rules update
  | isCorrectlyOrdered update rules = 0
  | otherwise =
      case sortUpdate update rules of
        Just sorted -> sorted !! (length sorted `div` 2)
        Nothing -> 0

isCorrectlyOrdered :: [Int] -> [([Int], [Int])] -> Bool
isCorrectlyOrdered update rules =
  let positions = Map.fromList $ zip update [0..]
  in all (\([x], [y]) ->
            case (Map.lookup x positions, Map.lookup y positions) of
              (Just px, Just py) -> px < py
              _ -> True
        ) rules

sortUpdate :: [Int] -> [([Int], [Int])] -> Maybe [Int]
sortUpdate update rules =
  let adj = buildAdj update rules
  in topologicalSort adj

buildAdj :: [Int] -> [([Int], [Int])] -> Map.Map Int [Int]
buildAdj update rules =
  let pages = Map.fromList $ zip update (repeat ())
      initialAdj = Map.fromList $ map (\x -> (x, [])) update
      addEdges adj ([x], [y]) =
        if Map.member x pages && Map.member y pages
          then Map.insertWith (++) x [y] adj
          else adj
  in foldl addEdges initialAdj rules

topologicalSort :: Map.Map Int [Int] -> Maybe [Int]
topologicalSort adj =
  let (visited, result) = tSort (Map.keys adj) Map.empty []
  in if length result == Map.size adj then Just result else Nothing
  where
    tSort :: [Int] -> Map.Map Int Bool -> [Int] -> ([Int], [Int])
    tSort [] visited result = (Map.keys visited, result)
    tSort (n:ns) visited result
      | Map.member n visited = tSort ns visited result
      | otherwise =
          case visit n Map.empty visited result of
            Just (visited', result') -> tSort ns visited' result'
            Nothing -> ([], [])

    visit :: Int -> Map.Map Int Bool -> Map.Map Int Bool -> [Int] -> Maybe (Map.Map Int Bool, [Int])
    visit n tempMarked visited result
      | Map.member n tempMarked = Nothing
      | Map.member n visited = Just (visited, result)
      | otherwise = do
          let tempMarked' = Map.insert n True tempMarked
          (visited', result') <- foldM (visit') (visited, result) (fromMaybe [] (Map.lookup n adj))
          let visited'' = Map.insert n True visited'
          return (visited'', n:result')

    visit' :: (Map.Map Int Bool, [Int]) -> Int -> Maybe (Map.Map Int Bool, [Int])
    visit' (visited, result) m = visit m Map.empty visited result

foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM f acc [] = return acc
foldM f acc (x:xs) = do
  acc' <- f acc x
  foldM f acc' xs
