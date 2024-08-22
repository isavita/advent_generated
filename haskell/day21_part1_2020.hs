import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

main = do
    contents <- readFile "input.txt"
    let foods = map parseFood (lines contents)
        allergenMap = buildAllergenMap foods
        allIngredients = concatMap fst foods
        safeIngredients = filter (`notElem` concatMap Set.toList (Map.elems allergenMap)) allIngredients
    print $ length safeIngredients

parseFood :: String -> ([String], [String])
parseFood line = (ingredients, allergens)
    where (ingredientsPart, allergensPart) = break (== '(') line
          ingredients = words ingredientsPart
          allergens = map (filter (/= ',')) $ words $ drop 10 $ init allergensPart

buildAllergenMap :: [([String], [String])] -> Map.Map String (Set.Set String)
buildAllergenMap foods = foldl updateMap Map.empty foods
    where updateMap acc (ingredients, allergens) = foldl (updateAllergen ingredients) acc allergens
          updateAllergen ingredients acc allergen = Map.insertWith Set.intersection allergen (Set.fromList ingredients) acc