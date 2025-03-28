
{-# LANGUAGE BangPatterns #-} -- Enable strictness annotations for potential performance improvements

module Main (main) where

-- Import necessary modules from base and containers
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map) -- Use strict maps for efficiency
import qualified Data.Set as S
import Data.Set (Set)         -- Sets are ideal for ingredients and allergens
import Data.List (intercalate, sortBy, foldl', lines, break, takeWhile, drop, length, dropWhile, dropWhileEnd, isPrefixOf)
import Data.Char (isSpace)
import Data.Ord (comparing)    -- For sorting pairs by key
import System.IO (readFile)    -- To read the input file
import Control.Exception (assert) -- For asserting conditions (e.g., finding singletons)

-- Helper function to trim leading/trailing whitespace from a string
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- Parses a single line of the input file.
-- Example line: "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
-- Returns a tuple containing:
--   1. A Set of ingredient strings for the food (e.g., {"mxmxvkd", "kfcds", ...}).
--   2. A Set of allergen strings listed for the food (e.g., {"dairy", "fish"}).
parseLine :: String -> (Set String, Set String)
parseLine line = case break (== '(') line of
    (ingredientsPart, allergensPart) ->
        -- Process ingredients: trim whitespace, split by space, convert to a Set
        let !ingredients = S.fromList . words $ trim ingredientsPart
            -- Process allergens: check if the "(contains " prefix exists
            !allergens = if not ("(contains " `isPrefixOf` allergensPart)
                        then S.empty -- No allergens listed or unexpected format
                        else
                            -- Extract the content between "(contains " and ")"
                            let content = takeWhile (/= ')') $ drop (length "(contains ") allergensPart
                            -- Replace commas with spaces, split into words, convert to Set.
                            in S.fromList . words $ map (\c -> if c == ',' then ' ' else c) content
         in (ingredients, allergens)

-- Builds the initial map from each allergen to the set of ingredients that
-- *could potentially* contain that allergen.
-- An ingredient is a potential candidate for an allergen if it appears in
-- *every* food list where that allergen is mentioned.
-- This uses `Map.fromListWith` combined with `S.intersection`, which elegantly
-- groups by allergen and finds the common ingredients across all lists for that allergen.
buildCandidates :: [(Set String, Set String)] -> Map String (Set String)
buildCandidates foods = Map.fromListWith S.intersection $ do
    -- Iterate through each food item (ingredients set, allergens set)
    (!ingredients, !allergens) <- foods
    -- Iterate through each allergen listed for the current food
    allergen <- S.toList allergens
    -- Create a pair (allergen, ingredient_set). `fromListWith` will group these
    -- pairs by allergen and apply `S.intersection` to the associated ingredient sets.
    return (allergen, ingredients)

-- Recursively determines the unique ingredient for each allergen.
-- It works by repeatedly finding an allergen that maps to only one possible ingredient
-- (a singleton set in the candidate map). It assigns that mapping, removes the
-- identified allergen and ingredient from further consideration, and recurses.
solveMapping :: Map String (Set String) -> Map String String
solveMapping candidates
    -- Base case: If the candidate map is empty, all allergens have been assigned.
    | Map.null candidates = Map.empty
    | otherwise =
        -- Find all allergens that currently map to a singleton set of ingredients.
        let singletons = Map.filter (\s -> S.size s == 1) candidates
        in -- Assertion: The problem statement implies a unique solution exists and can be
           -- found using this iterative singleton-elimination method. This assertion
           -- verifies that we always find at least one singleton at each step.
           assert (not (Map.null singletons)) $
             -- Take the first such singleton found (the order doesn't affect the final result).
             -- `Map.elemAt 0` gets the first element (key-value pair) from the map.
             let (!solvedAllergen, !singletonSet) = Map.elemAt 0 singletons
                 -- `S.elemAt 0` gets the single element from the singleton set.
                 !solvedIngredient = S.elemAt 0 singletonSet

                 -- Prepare for the recursive call by reducing the problem size:
                 -- 1. Remove the now-solved allergen from the candidate map.
                 !remainingCandidates = Map.delete solvedAllergen candidates
                 -- 2. Remove the identified ingredient as a possibility from the candidate
                 --    sets of all *other* remaining allergens.
                 !updatedCandidates = Map.map (S.delete solvedIngredient) remainingCandidates

             -- Add the definitive mapping (solvedAllergen -> solvedIngredient) to the result
             -- obtained by recursively solving the smaller, updated problem.
             in Map.insert solvedAllergen solvedIngredient (solveMapping updatedCandidates)

-- Main entry point of the program.
main :: IO ()
main = do
    -- Read the entire content of the input file "input.txt".
    contents <- readFile "input.txt"

    -- Parse the input lines into a list of (Set Ingredient, Set Allergen) tuples.
    -- Using `foldl'` with a strict accumulator (`!acc`) helps prevent potential
    -- space leaks that could occur with lazy evaluation on large input files.
    let !foodData = foldl' (\ !acc line -> parseLine line : acc) [] (lines contents)

    -- --- Part 1: Count appearances of safe ingredients ---

    -- Build the initial map of allergens to potential ingredient candidates.
    let !initialCandidates = buildCandidates foodData

    -- Collect all ingredient occurrences across all foods (this list includes duplicates).
    let !allIngredientsList = concatMap (S.toList . fst) foodData
    -- Create a set containing all unique ingredients mentioned anywhere in the input.
    let !allUniqueIngredients = S.fromList allIngredientsList

    -- Identify the set of ingredients that are potential candidates for *any* allergen.
    -- This is achieved by taking the union of all candidate sets in the `initialCandidates` map.
    let !potentialAllergenicIngredients = S.unions (Map.elems initialCandidates)

    -- Determine the set of "safe" ingredients. These are the unique ingredients that
    -- do *not* appear in the set of potential allergenic ingredients.
    -- `S.\\` computes the set difference.
    let !safeIngredientSet = allUniqueIngredients S.\\ potentialAllergenicIngredients

    -- Count how many times any of the ingredients from the `safeIngredientSet` appear
    -- in the original `allIngredientsList` (which includes duplicates).
    let !safeIngredientCount = length $ filter (`S.member` safeIngredientSet) allIngredientsList

    -- Print the result for Part 1.
    putStrLn $ "Part 1: " ++ show safeIngredientCount

    -- --- Part 2: Determine the canonical dangerous ingredient list ---

    -- Use the `solveMapping` function with the initial candidates to find the
    -- definitive ingredient associated with each allergen.
    let !finalAllergenMap = solveMapping initialCandidates

    -- Convert the final map (allergen -> ingredient) into a list of (allergen, ingredient) pairs.
    let !allergenIngredientPairs = Map.toList finalAllergenMap

    -- Sort these pairs alphabetically based on the allergen name (the key of the pair).
    let !sortedPairs = sortBy (comparing fst) allergenIngredientPairs

    -- Extract just the ingredient names from the sorted pairs.
    let !dangerousIngredientsSorted = map snd sortedPairs

    -- Join the sorted ingredient names with commas to produce the final canonical list string.
    let !canonicalDangerousList = intercalate "," dangerousIngredientsSorted

    -- Print the result for Part 2.
    putStrLn $ "Part 2: " ++ canonicalDangerousList
