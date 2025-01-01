
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
bossHP = parseInt(input[0].split(': ')[1])
bossDamage = parseInt(input[1].split(': ')[1])

minMana = Infinity

simulate = (state, playerTurn) ->
  if state.manaSpent >= minMana
    return
  if state.bossHP <= 0
    minMana = state.manaSpent
    return
  if state.playerHP <= 0
    return

  if playerTurn
    state.playerHP--
    if state.playerHP <= 0
      return

  if state.shieldTimer > 0
    state.shieldTimer--
  if state.poisonTimer > 0
    state.bossHP -= 3
    state.poisonTimer--
  if state.rechargeTimer > 0
    state.playerMana += 101
    state.rechargeTimer--

  if not playerTurn
    damage = state.bossDamage
    if state.shieldTimer > 0
      damage -= 7
    if damage < 1
      damage = 1
    state.playerHP -= damage
    simulate(state, true)
    return

  if state.playerMana >= 53
    newState = { ...state }
    newState.playerMana -= 53
    newState.manaSpent += 53
    newState.bossHP -= 4
    simulate(newState, false)
  if state.playerMana >= 73
    newState = { ...state }
    newState.playerMana -= 73
    newState.manaSpent += 73
    newState.bossHP -= 2
    newState.playerHP += 2
    simulate(newState, false)
  if state.playerMana >= 113 and state.shieldTimer == 0
    newState = { ...state }
    newState.playerMana -= 113
    newState.manaSpent += 113
    newState.shieldTimer = 6
    simulate(newState, false)
  if state.playerMana >= 173 and state.poisonTimer == 0
    newState = { ...state }
    newState.playerMana -= 173
    newState.manaSpent += 173
    newState.poisonTimer = 6
    simulate(newState, false)
  if state.playerMana >= 229 and state.rechargeTimer == 0
    newState = { ...state }
    newState.playerMana -= 229
    newState.manaSpent += 229
    newState.rechargeTimer = 5
    simulate(newState, false)

initialState = {
  playerHP: 50
  playerMana: 500
  bossHP: bossHP
  bossDamage: bossDamage
  shieldTimer: 0
  poisonTimer: 0
  rechargeTimer: 0
  manaSpent: 0
}

simulate(initialState, true)
console.log minMana
