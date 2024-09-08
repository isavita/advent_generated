require 'set'
require 'pqueue'

SPELLS = {
  magic_missile: { cost: 53, damage: 4 },
  drain: { cost: 73, damage: 2, heal: 2 },
  shield: { cost: 113, turns: 6, armor: 7 },
  poison: { cost: 173, turns: 6, damage: 3 },
  recharge: { cost: 229, turns: 5, mana: 101 }
}

class GameState
  attr_reader :player_hp, :player_mana, :boss_hp, :boss_damage, :mana_spent, :effects, :hard_mode

  def initialize(player_hp, player_mana, boss_hp, boss_damage, mana_spent = 0, effects = {}, hard_mode = false)
    @player_hp = player_hp
    @player_mana = player_mana
    @boss_hp = boss_hp
    @boss_damage = boss_damage
    @mana_spent = mana_spent
    @effects = effects
    @hard_mode = hard_mode
  end

  def apply_effects
    armor = 0
    @effects.each do |spell, turns|
      case spell
      when :shield
        armor = SPELLS[:shield][:armor]
      when :poison
        @boss_hp -= SPELLS[:poison][:damage]
      when :recharge
        @player_mana += SPELLS[:recharge][:mana]
      end
      @effects[spell] -= 1
    end
    @effects.delete_if { |_, turns| turns <= 0 }
    armor
  end

  def cast_spell(spell)
    return nil if @player_mana < SPELLS[spell][:cost] || (@effects[spell] && @effects[spell] > 1)

    new_state = self.dup
    new_state.instance_eval do
      @player_mana -= SPELLS[spell][:cost]
      @mana_spent += SPELLS[spell][:cost]
      
      case spell
      when :magic_missile, :drain
        @boss_hp -= SPELLS[spell][:damage]
        @player_hp += SPELLS[spell][:heal] if spell == :drain
      else
        @effects[spell] = SPELLS[spell][:turns]
      end
    end
    new_state
  end

  def boss_turn
    armor = apply_effects
    damage = [@boss_damage - armor, 1].max
    @player_hp -= damage
  end

  def player_turn
    @player_hp -= 1 if @hard_mode
    apply_effects
  end

  def game_over?
    @player_hp <= 0 || @boss_hp <= 0
  end

  def player_wins?
    @boss_hp <= 0
  end

  def <=>(other)
    @mana_spent <=> other.mana_spent
  end

  def hash
    [@player_hp, @player_mana, @boss_hp, @effects].hash
  end

  def eql?(other)
    self.hash == other.hash
  end
end

def find_least_mana(player_hp, player_mana, boss_hp, boss_damage, hard_mode = false)
  initial_state = GameState.new(player_hp, player_mana, boss_hp, boss_damage, 0, {}, hard_mode)
  queue = PQueue.new([initial_state]) { |a, b| a.mana_spent < b.mana_spent }
  seen_states = Set.new

  while !queue.empty?
    state = queue.pop

    next if seen_states.include?(state)
    seen_states.add(state)

    state.player_turn
    return state.mana_spent if state.player_wins?
    next if state.game_over?

    SPELLS.keys.each do |spell|
      new_state = state.cast_spell(spell)
      next if new_state.nil?

      new_state.boss_turn
      if new_state.player_wins?
        return new_state.mana_spent
      elsif !new_state.game_over?
        queue.push(new_state)
      end
    end
  end

  nil
end

# Read boss stats from input
boss_hp, boss_damage = File.readlines('input.txt').map { |line| line.split(': ').last.to_i }

# Part 1
puts "Part 1: #{find_least_mana(50, 500, boss_hp, boss_damage)}"

# Part 2
puts "Part 2: #{find_least_mana(50, 500, boss_hp, boss_damage, true)}"
