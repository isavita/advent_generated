
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Chemical = Text
type Amount = Int
type Reaction = (Amount, [(Amount, Chemical)])
type Reactions = M.Map Chemical Reaction
type Surplus = M.Map Chemical Amount

parseChemical :: Text -> (Amount, Chemical)
parseChemical s =
  let [a, n] = T.words s
  in (read (T.unpack a), n)

parseLine :: Text -> (Chemical, Reaction)
parseLine line =
  let [inputs, output] = T.splitOn " => " line
      (outAmount, outName) = parseChemical output
      inList = map parseChemical $ T.splitOn ", " inputs
  in (outName, (outAmount, inList))

calculateOre :: Reactions -> Surplus -> Chemical -> Amount -> (Amount, Surplus)
calculateOre reactions surplus chem amountNeeded =
  if chem == "ORE" then
    (amountNeeded, surplus)
  else
    case M.lookup chem surplus of
      Just surplusAmount | surplusAmount >= amountNeeded ->
        (0, M.insert chem (surplusAmount - amountNeeded) surplus)
      _ ->
        let
            surplusAmount = fromMaybe 0 (M.lookup chem surplus)
            amountNeeded' = amountNeeded - surplusAmount
            surplus' = M.insert chem 0 surplus
            (reactionAmount, ingredients) = fromMaybe (error "Reaction not found") (M.lookup chem reactions)
            times = (amountNeeded' + reactionAmount - 1) `div` reactionAmount
            (oreNeeded, newSurplus) = foldr f (0, surplus') ingredients
                where
                  f (ingredientAmount, ingredientName) (accOre, accSurplus) =
                    let (ore, surp) = calculateOre reactions accSurplus ingredientName (ingredientAmount * times)
                    in (accOre + ore, surp)
        in
            (oreNeeded, M.insert chem (times * reactionAmount - amountNeeded') newSurplus)

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just x) = x
  
main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let reactionList = map parseLine . T.lines $ contents
  let reactions = M.fromList reactionList
  let (ore, _) = calculateOre reactions M.empty "FUEL" 1
  print ore
