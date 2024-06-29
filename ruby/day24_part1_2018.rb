class Group
  attr_reader :army, :units, :hit_points, :attack_damage, :attack_type, :initiative
  attr_accessor :target

  def initialize(army, units, hit_points, attack_damage, attack_type, initiative, weaknesses, immunities)
    @army = army
    @units = units
    @hit_points = hit_points
    @attack_damage = attack_damage
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
      return count_units if battle_ended?

      target_selection
      attack
      
      # Clean up dead groups
      @armies.each { |_, army| army.reject! { |group| !group.alive? } }
    end
  end

  private

  def battle_ended?
    @armies.values.any?(&:empty?)
  end

  def count_units
    @armies.values.flatten.sum(&:units)
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
    groups.each do |attacker|
      if attacker.alive? && attacker.target
        damage = attacker.damage_to(attacker.target)
        attacker.target.receive_damage(damage)
        attacker.target = nil
      end
    end
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

input = File.read('input.txt')
armies = parse_input(input)
battle = Battle.new(armies[:immune_system], armies[:infection])
result = battle.fight

puts "The winning army has #{result} units."
