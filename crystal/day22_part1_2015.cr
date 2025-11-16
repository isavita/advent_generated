
class GameState
  property player_hp : Int32
  property player_mana : Int32
  property boss_hp : Int32
  property boss_damage : Int32
  property shield_timer : Int32
  property poison_timer : Int32
  property recharge_timer : Int32
  property mana_spent : Int32

  def initialize(@boss_hp, @boss_damage)
    @player_hp = 50
    @player_mana = 500
    @shield_timer = 0
    @poison_timer = 0
    @recharge_timer = 0
    @mana_spent = 0
  end

  def clone
    copy = GameState.new(@boss_hp, @boss_damage)
    copy.player_hp = @player_hp
    copy.player_mana = @player_mana
    copy.shield_timer = @shield_timer
    copy.poison_timer = @poison_timer
    copy.recharge_timer = @recharge_timer
    copy.mana_spent = @mana_spent
    copy
  end
end

def min_mana_to_win(initial_state)
  min_mana = Int32::MAX
  simulate = uninitialized Proc(GameState, Bool, Nil)
  simulate = ->(state : GameState, player_turn : Bool) {
    return if state.mana_spent >= min_mana
    if state.boss_hp <= 0
      min_mana = state.mana_spent
      return
    end
    return if state.player_hp <= 0

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
      damage = {damage, 1}.max
      state.player_hp -= damage
      simulate.call(state.clone, true)
      return
    end

    costs = [53, 73, 113, 173, 229]
    effects = [
      ->(s : GameState) { s.boss_hp -= 4 },
      ->(s : GameState) { s.boss_hp -= 2; s.player_hp += 2 },
      ->(s : GameState) { (s.shield_timer == 0) ? (s.shield_timer = 6) : (return false) },
      ->(s : GameState) { (s.poison_timer == 0) ? (s.poison_timer = 6) : (return false) },
      ->(s : GameState) { (s.recharge_timer == 0) ? (s.recharge_timer = 5) : (return false) }
    ]

    costs.each_with_index do |cost, i|
      next unless state.player_mana >= cost
      new_state = state.clone
      new_state.player_mana -= cost
      new_state.mana_spent += cost
      next if effects[i].call(new_state) == false
      simulate.call(new_state, false)
    end
  }
  simulate.call(initial_state.clone, true)
  min_mana
end

boss_hp = 0
boss_damage = 0
File.open("input.txt") do |f|
  boss_hp = f.gets.to_s.split.last.to_i
  boss_damage = f.gets.to_s.split.last.to_i
end

initial_state = GameState.new(boss_hp, boss_damage)
puts min_mana_to_win(initial_state)
