fs = require 'fs'

class GameState
  constructor: (@playerHP, @playerMana, @bossHP, @bossDamage, @shieldTimer=0, @poisonTimer=0, @rechargeTimer=0, @manaSpent=0) ->

minManaToWin = (initialState) ->
  minMana = Infinity
  simulate = (state, playerTurn) ->
    return if state.manaSpent >= minMana
    return if state.bossHP <= 0 and (minMana = state.manaSpent)
    return if state.playerHP <= 0

    # Apply effects
    state.shieldTimer-- if state.shieldTimer > 0
    if state.poisonTimer > 0
      state.bossHP -= 3
      state.poisonTimer--
    if state.rechargeTimer > 0
      state.playerMana += 101
      state.rechargeTimer--

    if not playerTurn
      damage = state.bossDamage
      damage -= 7 if state.shieldTimer > 0
      damage = 1 if damage < 1
      state.playerHP -= damage
      simulate state, true
      return

    if state.playerMana >= 53
      newState = new GameState(state.playerHP, state.playerMana - 53, state.bossHP - 4, state.bossDamage, state.shieldTimer, state.poisonTimer, state.rechargeTimer, state.manaSpent + 53)
      simulate newState, false
    if state.playerMana >= 73
      newState = new GameState(state.playerHP + 2, state.playerMana - 73, state.bossHP - 2, state.bossDamage, state.shieldTimer, state.poisonTimer, state.rechargeTimer, state.manaSpent + 73)
      simulate newState, false
    if state.playerMana >= 113 and state.shieldTimer == 0
      newState = new GameState(state.playerHP, state.playerMana - 113, state.bossHP, state.bossDamage, 6, state.poisonTimer, state.rechargeTimer, state.manaSpent + 113)
      simulate newState, false
    if state.playerMana >= 173 and state.poisonTimer == 0
      newState = new GameState(state.playerHP, state.playerMana - 173, state.bossHP, state.bossDamage, state.shieldTimer, 6, state.rechargeTimer, state.manaSpent + 173)
      simulate newState, false
    if state.playerMana >= 229 and state.rechargeTimer == 0
      newState = new GameState(state.playerHP, state.playerMana - 229, state.bossHP, state.bossDamage, state.shieldTimer, state.poisonTimer, 5, state.manaSpent + 229)
      simulate newState, false

  initialState.playerHP = 50
  initialState.playerMana = 500
  simulate initialState, true
  minMana

fs.readFile 'input.txt', 'utf8', (err, data) ->
  [bossHPLine, bossDamageLine] = data.trim().split '\n'
  bossHP = parseInt bossHPLine.split(': ')[1]
  bossDamage = parseInt bossDamageLine.split(': ')[1]

  initialState = new GameState(undefined, undefined, bossHP, bossDamage)
  console.log minManaToWin(initialState)