
class GameState
  attr_accessor :playerHP, :playerMana, :bossHP, :bossDamage, :shieldTimer, :poisonTimer, :rechargeTimer, :manaSpent

  def initialize(playerHP, playerMana, bossHP, bossDamage, shieldTimer, poisonTimer, rechargeTimer, manaSpent)
    @playerHP = playerHP
    @playerMana = playerMana
    @bossHP = bossHP
    @bossDamage = bossDamage
    @shieldTimer = shieldTimer
    @poisonTimer = poisonTimer
    @rechargeTimer = rechargeTimer
    @manaSpent = manaSpent
  end
end

def min_mana_to_win(initial_state)
  min_mana = 1 << 64
  simulate = lambda do |state, player_turn|
    return if state.manaSpent >= min_mana
    return min_mana = state.manaSpent if state.bossHP <= 0
    return if state.playerHP <= 0

    if state.shieldTimer > 0
      state.shieldTimer -= 1
    end
    if state.poisonTimer > 0
      state.bossHP -= 3
      state.poisonTimer -= 1
    end
    if state.rechargeTimer > 0
      state.playerMana += 101
      state.rechargeTimer -= 1
    end

    if !player_turn
      damage = state.bossDamage
      damage -= 7 if state.shieldTimer > 0
      damage = 1 if damage < 1
      state.playerHP -= damage
      simulate.call(state, true)
      return
    end

    if state.playerMana >= 53
      new_state = Marshal.load(Marshal.dump(state))
      new_state.playerMana -= 53
      new_state.manaSpent += 53
      new_state.bossHP -= 4
      simulate.call(new_state, false)
    end
    if state.playerMana >= 73
      new_state = Marshal.load(Marshal.dump(state))
      new_state.playerMana -= 73
      new_state.manaSpent += 73
      new_state.bossHP -= 2
      new_state.playerHP += 2
      simulate.call(new_state, false)
    end
    if state.playerMana >= 113 && state.shieldTimer == 0
      new_state = Marshal.load(Marshal.dump(state))
      new_state.playerMana -= 113
      new_state.manaSpent += 113
      new_state.shieldTimer = 6
      simulate.call(new_state, false)
    end
    if state.playerMana >= 173 && state.poisonTimer == 0
      new_state = Marshal.load(Marshal.dump(state))
      new_state.playerMana -= 173
      new_state.manaSpent += 173
      new_state.poisonTimer = 6
      simulate.call(new_state, false)
    end
    if state.playerMana >= 229 && state.rechargeTimer == 0
      new_state = Marshal.load(Marshal.dump(state))
      new_state.playerMana -= 229
      new_state.manaSpent += 229
      new_state.rechargeTimer = 5
      simulate.call(new_state, false)
    end
  end

  initial_state.playerHP = 50
  initial_state.playerMana = 500
  simulate.call(initial_state, true)
  min_mana
end

file = File.open("input.txt")
bossHP = file.readline.split(": ")[1].to_i
bossDamage = file.readline.split(": ")[1].to_i
initial_state = GameState.new(0, 0, bossHP, bossDamage, 0, 0, 0, 0)
puts min_mana_to_win(initial_state)
