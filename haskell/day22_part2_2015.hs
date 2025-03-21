
import System.IO
import Data.Maybe (fromMaybe)
import Data.IORef
import Control.Monad (when)

data GameState = GameState {
    playerHP :: Int,
    playerMana :: Int,
    bossHP :: Int,
    bossDamage :: Int,
    shieldTimer :: Int,
    poisonTimer :: Int,
    rechargeTimer :: Int,
    manaSpent :: Int
} deriving (Show)

minManaToWin :: Int -> Int -> IO Int
minManaToWin bossInitialHP bossInitialDamage = do
    minManaRef <- newIORef maxBound
    let initialState = GameState 50 500 bossInitialHP bossInitialDamage 0 0 0 0

    let simulate :: GameState -> Bool -> IO ()
        simulate state playerTurn = do
            currentMinMana <- readIORef minManaRef
            when (manaSpent state < currentMinMana) $ do
                if bossHP state <= 0 then do
                    writeIORef minManaRef (manaSpent state)
                else if playerHP state <= 0 then
                    return ()
                else do
                    let state' = if playerTurn then state { playerHP = playerHP state - 1 } else state
                    when (playerHP state' > 0) $ do
                        let shieldEffect = if shieldTimer state' > 0 then -7 else 0
                        let poisonEffect = if poisonTimer state' > 0 then -3 else 0
                        let rechargeEffect = if rechargeTimer state' > 0 then 101 else 0
                        
                        let stateAfterEffects = state' {
                            shieldTimer = max 0 (shieldTimer state' - 1),
                            poisonTimer = max 0 (poisonTimer state' - 1),
                            rechargeTimer = max 0 (rechargeTimer state' - 1),
                            bossHP = bossHP state' + poisonEffect,
                            playerMana = playerMana state' + rechargeEffect
                        }

                        if not playerTurn then do
                            let damage = max 1 (bossDamage stateAfterEffects + shieldEffect)
                            let newState = stateAfterEffects { playerHP = playerHP stateAfterEffects - damage }
                            simulate newState True
                        else do
                            when (playerMana stateAfterEffects >= 53) $ do
                                let newState = stateAfterEffects { playerMana = playerMana stateAfterEffects - 53, manaSpent = manaSpent stateAfterEffects + 53, bossHP = bossHP stateAfterEffects - 4 }
                                simulate newState False
                            when (playerMana stateAfterEffects >= 73) $ do
                                let newState = stateAfterEffects { playerMana = playerMana stateAfterEffects - 73, manaSpent = manaSpent stateAfterEffects + 73, bossHP = bossHP stateAfterEffects - 2, playerHP = playerHP stateAfterEffects + 2 }
                                simulate newState False
                            when (playerMana stateAfterEffects >= 113 && shieldTimer stateAfterEffects == 0) $ do
                                let newState = stateAfterEffects { playerMana = playerMana stateAfterEffects - 113, manaSpent = manaSpent stateAfterEffects + 113, shieldTimer = 6 }
                                simulate newState False
                            when (playerMana stateAfterEffects >= 173 && poisonTimer stateAfterEffects == 0) $ do
                                let newState = stateAfterEffects { playerMana = playerMana stateAfterEffects - 173, manaSpent = manaSpent stateAfterEffects + 173, poisonTimer = 6 }
                                simulate newState False
                            when (playerMana stateAfterEffects >= 229 && rechargeTimer stateAfterEffects == 0) $ do
                                let newState = stateAfterEffects { playerMana = playerMana stateAfterEffects - 229, manaSpent = manaSpent stateAfterEffects + 229, rechargeTimer = 5 }
                                simulate newState False
    simulate initialState True
    readIORef minManaRef

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let linesList = lines contents
    let bossHP = read . last . words $ head linesList :: Int
    let bossDamage = read . last . words $ linesList !! 1 :: Int
    
    result <- minManaToWin bossHP bossDamage
    print result
    hClose handle
