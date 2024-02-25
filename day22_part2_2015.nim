
import strutils

type
  GameState = object
    playerHP, playerMana, bossHP, bossDamage: int
    shieldTimer, poisonTimer, rechargeTimer: int
    manaSpent: int32

proc minManaToWin(initialState: var GameState): int32 =
  var minMana = int32.high
  proc simulate(state: var GameState, playerTurn: bool) =
    if state.manaSpent >= minMana:
      return
    if state.bossHP <= 0:
      minMana = state.manaSpent
      return
    if state.playerHP <= 0:
      return

    if playerTurn:
      dec(state.playerHP)
      if state.playerHP <= 0:
        return

    if state.shieldTimer > 0:
      dec(state.shieldTimer)
    if state.poisonTimer > 0:
      state.bossHP -= 3
      dec(state.poisonTimer)
    if state.rechargeTimer > 0:
      state.playerMana += 101
      dec(state.rechargeTimer)

    if not playerTurn:
      var damage = state.bossDamage
      if state.shieldTimer > 0:
        damage -= 7
      if damage < 1:
        damage = 1
      state.playerHP -= damage
      simulate(state, true)
      return

    if state.playerMana >= 53:
      var newState = state
      newState.playerMana -= 53
      newState.manaSpent += 53
      newState.bossHP -= 4
      simulate(newState, false)
    if state.playerMana >= 73:
      var newState = state
      newState.playerMana -= 73
      newState.manaSpent += 73
      newState.bossHP -= 2
      newState.playerHP += 2
      simulate(newState, false)
    if state.playerMana >= 113 and state.shieldTimer == 0:
      var newState = state
      newState.playerMana -= 113
      newState.manaSpent += 113
      newState.shieldTimer = 6
      simulate(newState, false)
    if state.playerMana >= 173 and state.poisonTimer == 0:
      var newState = state
      newState.playerMana -= 173
      newState.manaSpent += 173
      newState.poisonTimer = 6
      simulate(newState, false)
    if state.playerMana >= 229 and state.rechargeTimer == 0:
      var newState = state
      newState.playerMana -= 229
      newState.manaSpent += 229
      newState.rechargeTimer = 5
      simulate(newState, false)

  initialState.playerHP = 50
  initialState.playerMana = 500
  simulate(initialState, true)
  result = minMana

var file = open("input.txt")
var bossHP = parseInt(file.readLine().split(": ")[1])
var bossDamage = parseInt(file.readLine().split(": ")[1])
var initialState = GameState(bossHP: bossHP, bossDamage: bossDamage)
echo minManaToWin(initialState)
