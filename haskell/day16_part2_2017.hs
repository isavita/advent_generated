
import Data.List (elemIndex, cycle)
import Data.Maybe (fromJust)
import System.IO (readFile)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let moves = words $ map (\c -> if c == ',' then ' ' else c) content
        initial = ['a'..'p']
        cycleLen = findCycleLen moves initial
        finalPos = iterate (performMoves moves) initial !! (1000000000 `mod` cycleLen)
    putStrLn finalPos

findCycleLen :: [String] -> String -> Int
findCycleLen moves initial = go initial 0
  where
    go state count
        | state == initial && count > 0 = count
        | otherwise = go (performMoves moves state) (count + 1)

performMoves :: [String] -> String -> String
performMoves moves state = foldl perform state moves
  where
    perform s move
        | 's' == head move = spin s (read $ tail move)
        | 'x' == head move = let [a, b] = map read $ words $ map (\c -> if c == '/' then ' ' else c) (tail move) in exchange s a b
        | 'p' == head move = let [a, b] = map head $ words $ map (\c -> if c == '/' then ' ' else c) (tail move) in partner s a b

spin :: String -> Int -> String
spin s x = drop n s ++ take n s
  where n = length s - x `mod` length s

exchange :: String -> Int -> Int -> String
exchange s a b = let (x, y) = (s !! a, s !! b) in [if i == a then y else if i == b then x else c | (i, c) <- zip [0..] s]

partner :: String -> Char -> Char -> String
partner s a b = exchange s (fromJust (elemIndex a s)) (fromJust (elemIndex b s))
