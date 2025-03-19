
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
  let [amount, name] = T.words s
  in (read $ T.unpack amount, name)

parseLine :: Text -> (Chemical, Reaction)
parseLine line =
  let [inputs, output] = T.splitOn " => " line
      (outAmount, outName) = parseChemical output
      inList = map parseChemical $ T.splitOn ", " inputs
  in (outName, (outAmount, inList))

calculateOre :: Reactions -> Surplus -> Chemical -> Amount -> (Amount, Surplus)
calculateOre reactions surplus chem amount
  | chem == "ORE" = (amount, surplus)
  | otherwise =
    case M.lookup chem surplus of
      Just surpAmount | surpAmount >= amount ->
        (0, M.insert chem (surpAmount - amount) surplus)
      _ ->
        let
          surpAmount = M.findWithDefault 0 chem surplus
          amountNeeded = amount - surpAmount
          surplus' = M.insert chem 0 surplus
          (reactionAmount, ingredients) = reactions M.! chem
          times = (amountNeeded + reactionAmount - 1) `div` reactionAmount
          (oreNeeded, newSurplus) = foldl (\(accOre, accSurplus) (ingAmount, ingName) ->
            let (addOre, nextSurplus) = calculateOre reactions accSurplus ingName (ingAmount * times)
            in (accOre + addOre, nextSurplus)
            ) (0, surplus') ingredients
          newSurplus' = M.insert chem (times * reactionAmount - amountNeeded) newSurplus
        in
        (oreNeeded, newSurplus')

maxFuel :: Reactions -> Amount -> Amount
maxFuel reactions oreAvailable =
    go 0 oreAvailable M.empty
    where
      go low high initSurplus
        | low < high =
            let mid  = (low + high + 1) `div` 2
                (oreNeeded, _) = calculateOre reactions initSurplus "FUEL" mid
            in if oreNeeded > oreAvailable
               then go low (mid - 1) initSurplus
               else go mid high initSurplus

        | otherwise = low

main :: IO ()
main = do
  contents <- TIO.readFile "input.txt"
  let
    reactionsList = map parseLine . T.lines $ contents
    reactions = M.fromList reactionsList
    oreAvailable = 1000000000000
  print $ maxFuel reactions oreAvailable
