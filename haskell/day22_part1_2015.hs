
import System.IO
import Data.Maybe (mapMaybe)
import Data.List (foldl')

data GameState = GameState {
    playerHP      :: Int,
    playerMana    :: Int,
    bossHP        :: Int,
    bossDamage    :: Int,
    shieldTimer   :: Int,
    poisonTimer   :: Int,
    rechargeTimer :: Int,
    manaSpent     :: Int
} deriving (Show, Eq, Ord)

data Spell = MagicMissile | Drain | Shield | Poison | Recharge deriving (Eq, Show, Enum, Bounded)

allSpells :: [Spell]
allSpells = [minBound .. maxBound]

spellCost :: Spell -> Int
spellCost MagicMissile = 53
spellCost Drain        = 73
spellCost Shield       = 113
spellCost Poison       = 173
spellCost Recharge     = 229

applyEffects :: GameState -> GameState
applyEffects state =
    let shieldActive   = shieldTimer state > 0
        poisonActive   = poisonTimer state > 0
        rechargeActive = rechargeTimer state > 0

        bossHP'      = if poisonActive then bossHP state - 3 else bossHP state
        playerMana'  = if rechargeActive then playerMana state + 101 else playerMana state

        shieldTimer'   = max 0 (shieldTimer state - 1)
        poisonTimer'   = max 0 (poisonTimer state - 1)
        rechargeTimer' = max 0 (rechargeTimer state - 1)

    in state { bossHP = bossHP', playerMana = playerMana',
               shieldTimer = shieldTimer', poisonTimer = poisonTimer', rechargeTimer = rechargeTimer' }

bossAttack :: GameState -> GameState
bossAttack state =
    let armor   = if shieldTimer state > 0 then 7 else 0
        damage  = max 1 (bossDamage state - armor)
        playerHP' = playerHP state - damage
    in state { playerHP = playerHP' }

tryCastSpell :: Spell -> GameState -> Maybe GameState
tryCastSpell spell state =
    let cost = spellCost spell
    in if playerMana state < cost then Nothing else
        let newStateBase = state { playerMana = playerMana state - cost, manaSpent = manaSpent state + cost }
        in case spell of
            MagicMissile -> Just $ newStateBase { bossHP = bossHP newStateBase - 4 }
            Drain        -> Just $ newStateBase { bossHP = bossHP newStateBase - 2, playerHP = playerHP newStateBase + 2 }
            Shield       -> if shieldTimer state == 0 then Just $ newStateBase { shieldTimer = 6 } else Nothing
            Poison       -> if poisonTimer state == 0 then Just $ newStateBase { poisonTimer = 6 } else Nothing
            Recharge     -> if rechargeTimer state == 0 then Just $ newStateBase { rechargeTimer = 5 } else Nothing

search :: Int -> GameState -> Bool -> Int
search bestWinCost state playerTurn =
    if manaSpent state >= bestWinCost then bestWinCost else

    let stateAfterEffects = applyEffects state
    in if bossHP stateAfterEffects <= 0 then min bestWinCost (manaSpent state)
       else if playerHP stateAfterEffects <= 0 then bestWinCost

       else if playerTurn then
           let explore spell currentBest =
                   case tryCastSpell spell stateAfterEffects of
                       Nothing        -> currentBest
                       Just nextState -> if manaSpent nextState >= currentBest
                                         then currentBest
                                         else search currentBest nextState False
           in foldl' (flip explore) bestWinCost allSpells

       else
           let stateAfterAttack = bossAttack stateAfterEffects
           in search bestWinCost stateAfterAttack True

parseInput :: String -> (Int, Int)
parseInput content =
    let ls = lines content
        hp = read $ drop (length "Hit Points: ") (head ls)
        dmg = read $ drop (length "Damage: ") (ls !! 1)
    in (hp, dmg)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let (bossHP', bossDamage') = parseInput content
        initialState = GameState {
            playerHP = 50,
            playerMana = 500,
            bossHP = bossHP',
            bossDamage = bossDamage',
            shieldTimer = 0,
            poisonTimer = 0,
            rechargeTimer = 0,
            manaSpent = 0
        }
        minMana = search maxBound initialState True
    print minMana

