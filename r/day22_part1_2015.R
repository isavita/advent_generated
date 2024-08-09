minManaToWin <- function(bossHP, bossDamage) {
  minMana <- Inf
  simulate <- function(state, playerTurn) {
    if (state$manaSpent >= minMana) return()
    if (state$bossHP <= 0) {
      minMana <<- state$manaSpent
      return()
    }
    if (state$playerHP <= 0) return()

    # Apply effects
    if (state$shieldTimer > 0) state$shieldTimer <- state$shieldTimer - 1
    if (state$poisonTimer > 0) {
      state$bossHP <- state$bossHP - 3
      state$poisonTimer <- state$poisonTimer - 1
    }
    if (state$rechargeTimer > 0) {
      state$playerMana <- state$playerMana + 101
      state$rechargeTimer <- state$rechargeTimer - 1
    }

    if (!playerTurn) {
      damage <- state$bossDamage
      if (state$shieldTimer > 0) damage <- damage - 7
      if (damage < 1) damage <- 1
      state$playerHP <- state$playerHP - damage
      simulate(state, TRUE)
      return()
    }

    if (state$playerMana >= 53) {
      newState <- state
      newState$playerMana <- newState$playerMana - 53
      newState$manaSpent <- newState$manaSpent + 53
      newState$bossHP <- newState$bossHP - 4
      simulate(newState, FALSE)
    }
    if (state$playerMana >= 73) {
      newState <- state
      newState$playerMana <- newState$playerMana - 73
      newState$manaSpent <- newState$manaSpent + 73
      newState$bossHP <- newState$bossHP - 2
      newState$playerHP <- newState$playerHP + 2
      simulate(newState, FALSE)
    }
    if (state$playerMana >= 113 && state$shieldTimer == 0) {
      newState <- state
      newState$playerMana <- newState$playerMana - 113
      newState$manaSpent <- newState$manaSpent + 113
      newState$shieldTimer <- 6
      simulate(newState, FALSE)
    }
    if (state$playerMana >= 173 && state$poisonTimer == 0) {
      newState <- state
      newState$playerMana <- newState$playerMana - 173
      newState$manaSpent <- newState$manaSpent + 173
      newState$poisonTimer <- 6
      simulate(newState, FALSE)
    }
    if (state$playerMana >= 229 && state$rechargeTimer == 0) {
      newState <- state
      newState$playerMana <- newState$playerMana - 229
      newState$manaSpent <- newState$manaSpent + 229
      newState$rechargeTimer <- 5
      simulate(newState, FALSE)
    }
  }

  initialState <- list(
    playerHP = 50,
    playerMana = 500,
    bossHP = bossHP,
    bossDamage = bossDamage,
    shieldTimer = 0,
    poisonTimer = 0,
    rechargeTimer = 0,
    manaSpent = 0
  )
  simulate(initialState, TRUE)
  return(minMana)
}

input <- readLines("input.txt")
bossHP <- as.integer(strsplit(input[1], ": ")[[1]][2])
bossDamage <- as.integer(strsplit(input[2], ": ")[[1]][2])
print(minManaToWin(bossHP, bossDamage))