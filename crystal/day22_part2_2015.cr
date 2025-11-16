
struct GameState
  property player_hp, player_mana, boss_hp, boss_damage,
          shield_timer, poison_timer, recharge_timer, mana_spent

  def initialize(@player_hp : Int32, @player_mana : Int32, @boss_hp : Int32, @boss_damage : Int32,
                 @shield_timer : Int32, @poison_timer : Int32, @recharge_timer : Int32, @mana_spent : Int32)
  end
end

class Game
  @@min_mana = Int32::MAX

  def self.min_mana_to_win(state : GameState, player_turn : Bool) : Int32
    return @@min_mana if state.mana_spent >= @@min_mana
    if state.boss_hp <= 0
      @@min_mana = {@@min_mana, state.mana_spent}.min
      return @@min_mana
    end
    return @@min_mana if state.player_hp <= 0

    if player_turn
      state.player_hp -= 1
      return @@min_mana if state.player_hp <= 0
    end

    state.shield_timer -= 1 if state.shield_timer > 0
    if state.poison_timer > 0
      state.boss_hp -= 3
      state.poison_timer -= 1
    end
    if state.recharge_timer > 0
      state.player_mana += 101
      state.recharge_timer -= 1
    end

    unless player_turn
      damage = state.boss_damage
      damage -= 7 if state.shield_timer > 0
      damage = 1 if damage < 1
      state.player_hp -= damage
      min_mana_to_win(state, true)
      return @@min_mana
    end

    if state.player_mana >= 53
      new_state = GameState.new(
        state.player_hp, state.player_mana - 53, state.boss_hp - 4, state.boss_damage,
        state.shield_timer, state.poison_timer, state.recharge_timer, state.mana_spent + 53
      )
      min_mana_to_win(new_state, false)
    end
    if state.player_mana >= 73
      new_state = GameState.new(
        state.player_hp + 2, state.player_mana - 73, state.boss_hp - 2, state.boss_damage,
        state.shield_timer, state.poison_timer, state.recharge_timer, state.mana_spent + 73
      )
      min_mana_to_win(new_state, false)
    end
    if state.player_mana >= 113 && state.shield_timer == 0
      new_state = GameState.new(
        state.player_hp, state.player_mana - 113, state.boss_hp, state.boss_damage,
        6, state.poison_timer, state.recharge_timer, state.mana_spent + 113
      )
      min_mana_to_win(new_state, false)
    end
    if state.player_mana >= 173 && state.poison_timer == 0
      new_state = GameState.new(
        state.player_hp, state.player_mana - 173, state.boss_hp, state.boss_damage,
        state.shield_timer, 6, state.recharge_timer, state.mana_spent + 173
      )
      min_mana_to_win(new_state, false)
    end
    if state.player_mana >= 229 && state.recharge_timer == 0
      new_state = GameState.new(
        state.player_hp, state.player_mana - 229, state.boss_hp, state.boss_damage,
        state.shield_timer, state.poison_timer, 5, state.mana_spent + 229
      )
      min_mana_to_win(new_state, false)
    end
    @@min_mana
  end
end

lines = File.read_lines("input.txt")
boss_hp = lines[0].split(": ")[1].to_i
boss_damage = lines[1].split(": ")[1].to_i

initial_state = GameState.new(50, 500, boss_hp, boss_damage, 0, 0, 0, 0)
puts Game.min_mana_to_win(initial_state, true)
