
module Main where

import Data.List (nub)
import Data.Maybe (catMaybes)
import qualified Data.Set as S

-- Represents a replacement rule (e.g., "H => HO")
type Rule = (String, String)

-- Parses a single replacement rule from a string.
parseRule :: String -> Maybe Rule
parseRule str = case words str of
  [left, "=>", right] -> Just (left, right)
  _                  -> Nothing

-- Parses all replacement rules from the input.
parseRules :: [String] -> [Rule]
parseRules = catMaybes . map parseRule

-- Applies a single replacement rule to a molecule.
applyRule :: Rule -> String -> [String]
applyRule (from, to) molecule = go 0
  where
    lenFrom = length from
    lenMol  = length molecule

    go :: Int -> [String]
    go i
      | i > lenMol - lenFrom = []  -- No more possible replacements
      | take lenFrom (drop i molecule) == from =
          (take i molecule ++ to ++ drop (i + lenFrom) molecule) : go (i + 1)
      | otherwise = go (i + 1)

-- Applies all rules to a molecule and returns distinct results.
applyAllRules :: [Rule] -> String -> [String]
applyAllRules rules molecule = nub $ concatMap (`applyRule` molecule) rules

-- Efficient version using Sets for faster uniqueness check
applyAllRulesEfficient :: [Rule] -> String -> [String]
applyAllRulesEfficient rules molecule = S.toList $ S.fromList $ concatMap (`applyRule` molecule) rules
main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let rules = parseRules (init (init ls)) -- All lines except the last two
  let molecule = last ls  -- last line as the medicine molecule

  -- Using the efficient Set-based function
  let distinctMolecules = applyAllRulesEfficient rules molecule
  print (length distinctMolecules)
