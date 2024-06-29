class Group
  attr_reader :army, :hit_points, :attack_type, :initiative, :original_attack_damage
  attr_accessor :units, :attack_damage, :target

  def initialize(army, units, hit_points, attack_damage, attack_type, initiative, weaknesses, immunities)
    @army = army
    @units = units
    @hit_points = hit_points
    @attack_damage = attack_damage
    @original_attack_damage = attack_damage
    @attack_type = attack_type
    @initiative = initiative
    @weaknesses = weaknesses
    @immunities = immunities
    @target = nil
  end

  def effective_power
    @units * @attack_damage
  end

  def damage_to(defender)
    return 0 if defender.immune_to?(@attack_type)
    damage = effective_power
    damage *= 2 if defender.weak_to?(@attack_type)
    damage
  end

  def receive_damage(damage)
    units_lost = [damage / @hit_points, @units].min
    @units -= units_lost
    units_lost
  end

  def alive?
    @units > 0
  end

  def weak_to?(attack_type)
    @weaknesses.include?(attack_type)
  end

  def immune_to?(attack_type)
    @immunities.include?(attack_type)
  end
end

class Battle
  def initialize(immune_system, infection)
    @armies = {
      immune_system: immune_system,
      infection: infection
    }
  end

  def fight
    loop do
      return nil if stalemate?
      return winning_army if battle_ended?

      target_selection
      units_killed = attack
      
      break if units_killed == 0 # Stalemate detection

      # Clean up dead groups
      @armies.each { |_, army| army.reject! { |group| !group.alive? } }
    end
    nil # Stalemate
  end

  def boost_immune_system(boost)
    @armies[:immune_system].each do |group|
      group.attack_damage = group.original_attack_damage + boost
    end
  end

  private

  def stalemate?
    @armies.values.all? { |army| army.all? { |group| group.effective_power == 0 } }
  end

  def battle_ended?
    @armies.values.any?(&:empty?)
  end

  def winning_army
    winner = @armies.find { |_, army| !army.empty? }
    [winner[0], winner[1].sum(&:units)]
  end

  def target_selection
    groups = @armies.values.flatten.sort_by { |g| [-g.effective_power, -g.initiative] }
    targeted = Set.new

    groups.each do |attacker|
      defenders = @armies.values.flatten - @armies[attacker.army]
      target = defenders
        .reject { |d| targeted.include?(d) }
        .max_by { |d| [attacker.damage_to(d), d.effective_power, d.initiative] }

      if target && attacker.damage_to(target) > 0
        attacker.target = target
        targeted.add(target)
      end
    end
  end

  def attack
    groups = @armies.values.flatten.sort_by { |g| -g.initiative }
    total_units_killed = 0
    groups.each do |attacker|
      if attacker.alive? && attacker.target
        damage = attacker.damage_to(attacker.target)
        units_killed = attacker.target.receive_damage(damage)
        total_units_killed += units_killed
        attacker.target = nil
      end
    end
    total_units_killed
  end
end

def parse_input(input)
  armies = { immune_system: [], infection: [] }
  current_army = nil

  input.each_line do |line|
    case line.strip
    when "Immune System:"
      current_army = :immune_system
    when "Infection:"
      current_army = :infection
    when /^\d/
      units, hit_points, attack_damage, initiative = line.scan(/\d+/).map(&:to_i)
      attack_type = line[/(\w+) damage/,1]
      weaknesses = line[/weak to ([\w, ]+)/, 1]&.split(", ") || []
      immunities = line[/immune to ([\w, ]+)/, 1]&.split(", ") || []
      
      armies[current_army] << Group.new(
        current_army, units, hit_points, attack_damage, attack_type, initiative, weaknesses, immunities
      )
    end
  end

  armies
end

def find_minimum_boost(immune_system, infection)
  low = 0
  high = 10000 # Adjust this upper bound if necessary

  while low <= high
    boost = (low + high) / 2
    battle = Battle.new(deep_copy(immune_system), deep_copy(infection))
    battle.boost_immune_system(boost)
    result = battle.fight

    if result && result[0] == :immune_system
      high = boost - 1
    else
      low = boost + 1
    end
  end

  low
end

def deep_copy(array)
  array.map { |group| Group.new(group.army, group.units, group.hit_points, group.original_attack_damage, group.attack_type, group.initiative, group.instance_variable_get(:@weaknesses), group.instance_variable_get(:@immunities)) }
end

input = File.read('input.txt')
armies = parse_input(input)

minimum_boost = find_minimum_boost(armies[:immune_system], armies[:infection])
final_battle = Battle.new(deep_copy(armies[:immune_system]), deep_copy(armies[:infection]))
final_battle.boost_immune_system(minimum_boost)
result = final_battle.fight

puts "The smallest boost needed for the Immune System to win is: #{minimum_boost}"
puts "The Immune System is left with #{result[1]} units after winning."
