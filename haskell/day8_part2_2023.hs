
import qualified Data.Map as M
import Data.List (foldl')

parseLine :: String -> (String, (String, String))
parseLine line = (head, (children !! 0, children !! 1))
  where
    parts = words $ map (\c -> if c `elem` "=(,)" then ' ' else c) line
    head = parts !! 0
    children = tail parts

parseInput :: [String] -> (String, M.Map String (String, String))
parseInput (instructions : _ : nodes) =
  (instructions, M.fromList $ map parseLine nodes)

gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

lcm' :: Int -> Int -> Int
lcm' a b = (a * b) `div` gcd' a b

lcmList :: [Int] -> Int
lcmList = foldl' lcm' 1

solve :: [String] -> Int
solve input = lcmList steps
  where
    (instructions, nodes) = parseInput input
    starts = filter (\k -> last k == 'A') $ M.keys nodes
    instructionsLength = length instructions
    steps = map (countSteps nodes instructions instructionsLength) starts

countSteps :: M.Map String (String, String) -> String -> Int -> String -> Int
countSteps nodes instructions instructionsLength start = go start 0
    where
        go element count
            | last element == 'Z' = count
            | otherwise = go nextElement (count + 1)
            where
                instruction = instructions !! (count `mod` instructionsLength)
                nextElement = case instruction of
                    'L' -> fst (nodes M.! element)
                    'R' -> snd (nodes M.! element)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let inputLines = lines input
  print $ solve inputLines
