import Data.List
import Data.Maybe
import System.IO

data Item = Item { name :: String, cost :: Int, damage :: Int, armor :: Int } deriving (Show, Eq)

weapons = [Item "Dagger" 8 4 0, Item "Shortsword" 10 5 0, Item "Warhammer" 25 6 0, Item "Longsword" 40 7 0, Item "Greataxe" 74 8 0]
armors = [Item "None" 0 0 0, Item "Leather" 13 0 1, Item "Chainmail" 31 0 2, Item "Splintmail" 53 0 3, Item "Bandedmail" 75 0 4, Item "Platemail" 102 0 5]
rings = [Item "None" 0 0 0, Item "Damage +1" 25 1 0, Item "Damage +2" 50 2 0, Item "Damage +3" 100 3 0, Item "Defense +1" 20 0 1, Item "Defense +2" 40 0 2, Item "Defense +3" 80 0 3]

main = do
    contents <- readFile "input.txt"
    let [bossHp, bossDamage, bossArmor] = map (read . last . words) $ lines contents
        maxGold = maximum [wCost + aCost + r1Cost + r2Cost | w <- weapons, a <- armors, r1 <- rings, r2 <- rings, r1 /= r2, let wCost = cost w, let aCost = cost a, let r1Cost = cost r1, let r2Cost = cost r2, not (playerWins 100 (damage w + damage r1 + damage r2) (armor a + armor r1 + armor r2) bossHp bossDamage bossArmor)]
    print maxGold

playerWins :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
playerWins playerHp playerDamage playerArmor bossHp bossDamage bossArmor = roundsToKillBoss <= roundsToKillPlayer
    where roundsToKillBoss = (bossHp + max 1 (playerDamage - bossArmor) - 1) `div` max 1 (playerDamage - bossArmor)
          roundsToKillPlayer = (playerHp + max 1 (bossDamage - playerArmor) - 1) `div` max 1 (bossDamage - playerArmor)