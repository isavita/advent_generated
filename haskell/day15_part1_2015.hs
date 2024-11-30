{-# LANGUAGE RecordWildCards #-}

import System.IO (readFile)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Monad (replicateM)

-- | Data structure to represent an ingredient and its properties
data Ingredient = Ingredient
  { capacity    :: Int
  , durability  :: Int
  , flavor      :: Int
  , texture     :: Int
  , calories    :: Int
  } deriving (Show)

-- | Parses a single line of input into an Ingredient
parseIngredient :: String -> Ingredient
parseIngredient line =
  let wordsList = words line
      -- Extract numerical values, ignoring commas and colons
      capacityVal   = read (filter (/= ',') (wordsList !! 2)) :: Int
      durabilityVal = read (filter (/= ',') (wordsList !! 4)) :: Int
      flavorVal     = read (filter (/= ',') (wordsList !! 6)) :: Int
      textureVal    = read (filter (/= ',') (wordsList !! 8)) :: Int
      caloriesVal   = read (wordsList !! 10) :: Int
  in Ingredient capacityVal durabilityVal flavorVal textureVal caloriesVal

-- | Generates all possible allocations of 'total' teaspoons among 'n' ingredients
allocate :: Int -> Int -> [[Int]]
allocate 1 total = [[total]]
allocate n total =
  [ x : xs
  | x <- [0..total]
  , xs <- allocate (n - 1) (total - x)
  ]

-- | Computes the total score for a given allocation of teaspoons
computeScore :: [Int] -> [Ingredient] -> Int
computeScore allocation ingredients =
  let totalCapacity   = max 0 (sum $ zipWith (*) allocation (map capacity ingredients))
      totalDurability = max 0 (sum $ zipWith (*) allocation (map durability ingredients))
      totalFlavor     = max 0 (sum $ zipWith (*) allocation (map flavor ingredients))
      totalTexture    = max 0 (sum $ zipWith (*) allocation (map texture ingredients))
  in totalCapacity * totalDurability * totalFlavor * totalTexture

-- | Main function to read input, process allocations, and determine the maximum score
main :: IO ()
main = do
  -- Read and parse the input file
  content <- readFile "input.txt"
  let ingredientLines = lines content
      ingredients = map parseIngredient ingredientLines
      numIngredients = length ingredients

  -- Generate all possible allocations of 100 teaspoons among the ingredients
  let allocations = allocate numIngredients 100

  -- Compute scores for all allocations
      scores = map (`computeScore` ingredients) allocations

  -- Determine the maximum score
      maxScore = maximum scores

  -- Output the result
  putStrLn $ "The highest possible cookie score is: " ++ show maxScore
