
import Control.Monad (forM_)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import System.IO (readFile)

data Ingredient = Ingredient { name :: String, capacity :: Int, durability :: Int, flavor :: Int, texture :: Int, calories :: Int }

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let ingredients = parseIngredients contents
    print $ findMaxScore ingredients 100 500

parseIngredients :: String -> [Ingredient]
parseIngredients = mapMaybe parseLine . lines
  where
    parseLine line = case words line of
        [n, _, c, _, d, _, f, _, t, _, cal] -> Just $ Ingredient n (read $ init c) (read $ init d) (read $ init f) (read $ init t) (read cal)
        _ -> Nothing

findMaxScore :: [Ingredient] -> Int -> Int -> Int
findMaxScore ingredients totalTeaspoons targetCalories = maximum $ calculateScores ingredients totalTeaspoons targetCalories

calculateScores :: [Ingredient] -> Int -> Int -> [Int]
calculateScores ingredients totalTeaspoons targetCalories = 
    [ score | tsp <- combinations (length ingredients) totalTeaspoons, 
              calculateCalories ingredients tsp == targetCalories,
              let score = calculateScore ingredients tsp ]

combinations :: Int -> Int -> [[Int]]
combinations n total = go n total
  where
    go 1 total = [[total]]
    go k total = [x : xs | x <- [0..total], xs <- go (k-1) (total - x)]

calculateScore :: [Ingredient] -> [Int] -> Int
calculateScore ingredients teaspoons = 
    let (cap, dur, fla, tex) = foldl (\(c, d, f, t) (i, tsp) -> (c + capacity (ingredients !! i) * tsp, d + durability (ingredients !! i) * tsp, f + flavor (ingredients !! i) * tsp, t + texture (ingredients !! i) * tsp)) (0, 0, 0, 0) (zip [0..] teaspoons)
    in max 0 (cap * dur * fla * tex)

calculateCalories :: [Ingredient] -> [Int] -> Int
calculateCalories ingredients teaspoons = sum $ zipWith (\i tsp -> calories (ingredients !! i) * tsp) [0..] teaspoons
