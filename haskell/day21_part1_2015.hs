{-# LANGUAGE RecordWildCards #-}

import System.IO (readFile)
import Data.List (minimumBy, subsequences)
import Data.Ord (comparing)
import Control.Monad (guard)

-- | Data structure to represent an item with its properties
data Item = Item
  { name    :: String  -- ^ Name of the item
  , cost    :: Int     -- ^ Cost in gold
  , damage  :: Int     -- ^ Damage provided by the item
  , armor   :: Int     -- ^ Armor provided by the item
  } deriving (Show, Eq)  -- Derived Eq for comparison

-- | Data structure to represent a character (player or boss)
data Character = Character
  { hp  :: Int  -- ^ Hit Points
  , dmg :: Int  -- ^ Damage
  , arm :: Int  -- ^ Armor
  } deriving (Show)

-- | Define the list of weapons, armors, and rings available in the shop
weapons :: [Item]
weapons =
  [ Item "Dagger"        8     4       0
  , Item "Shortsword"   10     5       0
  , Item "Warhammer"    25     6       0
  , Item "Longsword"    40     7       0
  , Item "Greataxe"     74     8       0
  ]

armors :: [Item]
armors =
  [ Item "Leather"      13     0       1
  , Item "Chainmail"    31     0       2
  , Item "Splintmail"   53     0       3
  , Item "Bandedmail"   75     0       4
  , Item "Platemail"   102     0       5
  ]

rings :: [Item]
rings =
  [ Item "Damage +1"    25     1       0
  , Item "Damage +2"    50     2       0
  , Item "Damage +3"   100     3       0
  , Item "Defense +1"   20     0       1
  , Item "Defense +2"   40     0       2
  , Item "Defense +3"   80     0       3
  ]

-- | Parse the boss's stats from input lines using pattern matching
parseBoss :: [String] -> Either String Character
parseBoss (hpLine:dmgLine:armLine:_) =
  case (parseStat hpLine "Hit Points:", parseStat dmgLine "Damage:", parseStat armLine "Armor:") of
    (Right hpVal, Right dmgVal, Right armVal) ->
      Right $ Character hpVal dmgVal armVal
    _ -> Left "Error parsing boss stats."
  where
    parseStat :: String -> String -> Either String Int
    parseStat line statName =
      let prefix = statName
          rest = drop (length prefix + 1) line  -- +1 for the colon
          valueStr = words rest !! 0
      in case reads valueStr of
           [(val, "")] -> Right val
           _           -> Left $ "Invalid value for " ++ statName
parseBoss _ = Left "Input does not contain enough lines for boss stats."

-- | Generate all possible valid equipment combinations
-- Must have exactly one weapon
-- Can have zero or one armor
-- Can have zero, one, or two rings (no duplicates)
allCombinations :: [Item] -> [Item] -> [Item] -> [[Item]]
allCombinations ws as rs =
  [ [w] ++ maybeToList a ++ rs'
  | w <- ws
  , a <- Nothing : map Just as
  , rs' <- ringCombinations rs
  ]
  where
    -- Generate all possible ring combinations (0, 1, or 2 rings)
    ringCombinations :: [Item] -> [[Item]]
    ringCombinations items =
      let ringsZero = [[]]
          ringsOne  = [[x] | x <- items]
          ringsTwo  = [ [x,y] | x <- items, y <- items, x /= y ]
      in ringsZero ++ ringsOne ++ ringsTwo

    -- Helper function to convert Maybe Item to [Item]
    maybeToList :: Maybe Item -> [Item]
    maybeToList Nothing  = []
    maybeToList (Just x) = [x]

-- | Simulate the fight between player and boss
-- Returns True if the player wins, False otherwise
simulateFight :: Character -> Character -> Bool
simulateFight player boss =
  let playerDmgPerTurn = max 1 (dmg player - arm boss)
      bossDmgPerTurn   = max 1 (dmg boss - arm player)
      turnsToKillBoss  = ceiling (fromIntegral (hp boss) / fromIntegral playerDmgPerTurn :: Double)
      turnsToKillPlayer= ceiling (fromIntegral (hp player) / fromIntegral bossDmgPerTurn :: Double)
  in turnsToKillBoss <= turnsToKillPlayer

-- | Main function to execute the program
main :: IO ()
main = do
  -- Read the input file
  content <- readFile "input.txt"
  let inputLines = lines content
      bossResult = parseBoss inputLines

  case bossResult of
    Left errMsg -> putStrLn errMsg
    Right boss -> do
      -- Define the player's base stats
      let basePlayer = Character 100 0 0

      -- Generate all possible equipment combinations
          combos = allCombinations weapons armors rings

      -- For each combination, calculate total cost, damage, and armor
          stats = [ (sum (map cost items), sum (map damage items), sum (map armor items)) | items <- combos ]

      -- Filter combinations where the player wins the fight
          winningCombos = [ (c, d, a) | (c, d, a) <- stats
                                    , let player = basePlayer { dmg = d, arm = a }
                                    , simulateFight player boss ]

      -- Ensure there is at least one winning combination
          minCost = if null winningCombos
                    then error "No winning combinations found."
                    else minimum [ c | (c, _, _) <- winningCombos ]

      -- Output the result
      putStrLn $ "The least amount of gold you can spend and still win the fight is: " ++ show minCost
