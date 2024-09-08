require 'set'
require 'algorithms'

SPELLS = {
  magic_missile: { cost: 53, damage: 4 },
  drain: { cost: 73, damage: 2, heal: 2 },
  shield: { cost: 113, duration: 6 },
  poison: { cost: 173, duration: 6 },
  recharge: { cost: 229, duration: 5 }
}

State = Struct.new(:player_hp, :player_mana, :boss_hp, :boss_damage, :mana_spent, :effects, :player_turn)

def simulate_turn(state, spell = nil)
  state = state.dup
  state.effects = state.effects.transform_values { |v| v.dup }

  # Apply effects
  state.player_mana += state.effects[:recharge] * 101
  state.boss_hp -= state.effects[:poison] * 3
  armor = state.effects[:shield] > 0 ? 7 : 0

  state.effects.transform_values! { |v| [v - 1, 0].max }

  return state if state.boss_hp <= 0

  if state.player_turn
    return nil if spell.nil? || SPELLS[spell][:cost] > state.player_mana
    state.player_mana -= SPELLS[spell][:cost]
    state.mana_spent += SPELLS[spell][:cost]

    case spell
    when :magic_missile
      state.boss_hp -= SPELLS[spell][:damage]
    when :drain
      state.boss_hp -= SPELLS[spell][:damage]
      state.player_hp += SPELLS[spell][:heal]
    when :shield, :poison, :recharge
      return nil if state.effects[spell] > 0
      state.effects[spell] = SPELLS[spell][:duration]
    end
  else
    damage = [state.boss_damage - armor, 1].max
    state.player_hp -= damage
  end

  state.player_turn = !state.player_turn
  state.player_hp <= 0 ? nil : state
end

def solve(player_hp, player_mana, boss_hp, boss_damage)
  initial_state = State.new(player_hp, player_mana, boss_hp, boss_damage, 0, Hash.new(0), true)
  queue = Containers::PriorityQueue.new { |x, y| x.mana_spent < y.mana_spent }
  queue.push(initial_state, 0)
  seen = Set.new

  while !queue.empty?
    state = queue.pop

    next if seen.include?([state.player_hp, state.player_mana, state.boss_hp, state.mana_spent, state.effects.hash, state.player_turn])
    seen.add([state.player_hp, state.player_mana, state.boss_hp, state.mana_spent, state.effects.hash, state.player_turn])

    if state.player_turn
      SPELLS.keys.each do |spell|
        next_state = simulate_turn(state, spell)
        return next_state.mana_spent if next_state && next_state.boss_hp <= 0
        queue.push(next_state, next_state.mana_spent) if next_state
      end
    else
      next_state = simulate_turn(state)
      queue.push(next_state, next_state.mana_spent) if next_state
    end
  end
end

# Read input from file
boss_hp, boss_damage = File.read('input.txt').split("\n").map { |line| line.split(': ').last.to_i }

# Solve and print result
puts solve(50, 500, boss_hp, boss_damage)
