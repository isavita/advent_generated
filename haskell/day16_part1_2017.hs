
import Data.List (elemIndex, intercalate)
import Data.Maybe (fromJust)
import System.IO (readFile)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let moves = words $ map (\c -> if c == ',' then ' ' else c) content
        programs = "abcdefghijklmnop"
        result = foldl performMove programs moves
    putStrLn result

performMove :: String -> String -> String
performMove ps move
    | 's' == head move = spin ps (read $ tail move)
    | 'x' == head move = let [a, b] = map read $ words $ map (\c -> if c == '/' then ' ' else c) (tail move) in exchange ps a b
    | 'p' == head move = let [a, b] = map head $ words $ map (\c -> if c == '/' then ' ' else c) (tail move) in partner ps a b

spin :: String -> Int -> String
spin ps x = drop n ps ++ take n ps
  where n = length ps - x `mod` length ps

exchange :: String -> Int -> Int -> String
exchange ps a b = let (x, y) = (ps !! a, ps !! b) in [if i == a then y else if i == b then x else p | (i, p) <- zip [0..] ps]

partner :: String -> Char -> Char -> String
partner ps a b = exchange ps (fromJust $ elemIndex a ps) (fromJust $ elemIndex b ps)
