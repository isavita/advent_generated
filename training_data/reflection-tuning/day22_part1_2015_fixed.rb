class GameState
  attr_accessor :player_hp, :player_mana, :boss_hp, :boss_damage, :effects, :mana_spent

  def initialize(player_hp, player_mana, boss_hp, boss_damage)
    @player_hp = player_hp
    @player_mana = player_mana
    @boss_hp = boss_hp
    @boss_damage = boss_damage
    @effects = {}
    @mana_spent = 0
  end

  def clone
    new_state = GameState.new(@player_hp, @player_mana, @boss_hp, @boss_damage)
    new_state.effects = @effects.clone
    new_state.mana_spent = @mana_spent
    new_state
  end
end

SPELLS = {
  magic_missile: { cost: 53, damage: 4 },
  drain: { cost: 73, damage: 2, heal: 2 },
  shield: { cost: 113, duration: 6 },
  poison: { cost: 173, duration: 6 },
  recharge: { cost: 229, duration: 5 }
}

def apply_effects(state)
  state.effects.each do |effect, timer|
    case effect
    when :shield
      # Shield effect is handled in boss_turn
    when :poison
      state.boss_hp -= 3
    when :recharge
      state.player_mana += 101
    end
    state.effects[effect] -= 1
  end
  state.effects.delete_if { |_, timer| timer <= 0 }
end

def cast_spell(state, spell)
  return nil if SPELLS[spell][:cost] > state.player_mana
  return nil if state.effects.key?(spell) && state.effects[spell] > 1

  new_state = state.clone
  new_state.player_mana -= SPELLS[spell][:cost]
  new_state.mana_spent += SPELLS[spell][:cost]

  case spell
  when :magic_missile
    new_state.boss_hp -= SPELLS[spell][:damage]
  when :drain
    new_state.boss_hp -= SPELLS[spell][:damage]
    new_state.player_hp += SPELLS[spell][:heal]
  when :shield, :poison, :recharge
    new_state.effects[spell] = SPELLS[spell][:duration]
  end

  new_state
end

def boss_turn(state)
  apply_effects(state)
  return state if state.boss_hp <= 0

  damage = state.boss_damage - (state.effects[:shield] ? 7 : 0)
  damage = 1 if damage < 1
  state.player_hp -= damage
  state
end

def find_least_mana(initial_state)
  queue = [initial_state]
  min_mana = Float::INFINITY

  while !queue.empty?
    state = queue.shift
    apply_effects(state)

    next if state.player_hp <= 0
    if state.boss_hp <= 0
      min_mana = [min_mana, state.mana_spent].min
      next
    end

    SPELLS.keys.each do |spell|
      new_state = cast_spell(state, spell)
      next if new_state.nil? || new_state.mana_spent >= min_mana

      new_state = boss_turn(new_state)
      queue << new_state if new_state.player_hp > 0
    end
  end

  min_mana
end

# Example usage:
initial_state = GameState.new(50, 500, 55, 8)  # Replace with actual boss stats
least_mana = find_least_mana(initial_state)
puts "Least amount of mana to win: #{least_mana}"
