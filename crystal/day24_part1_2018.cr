
class Group
  property units : Int32
  property hit_points : Int32
  property attack_damage : Int32
  property attack_type : String
  property initiative : Int32
  property immunities : Array(String)
  property weaknesses : Array(String)
  property attacker : Group?
  property target : Group?

  def initialize(@units, @hit_points, @attack_damage, @attack_type, @initiative, @immunities, @weaknesses)
    @attacker = nil
    @target = nil
  end

  def effective_power
    units * attack_damage
  end

  def damage_dealt(enemy)
    return 0 if enemy.immunities.includes?(attack_type)
    return effective_power * 2 if enemy.weaknesses.includes?(attack_type)
    effective_power
  end
end

class Initiative
  property groups : Array(Group)

  def initialize(@groups)
  end

  def attack
    groups.sort_by! { |g| -g.initiative }
    groups.each do |group|
      if group.units > 0 && group.target && group.target.not_nil!.units > 0
        t = group.target.not_nil!
        t.units -= group.damage_dealt(t) // t.hit_points
        t.units = 0 if t.units < 0
      end
      if (t = group.target)
        t.attacker = nil
        group.target = nil
      end
    end
  end

  def clean
    groups.select! { |g| g.units > 0 }
    groups.sort_by! { |g| -g.initiative }
  end
end

class Army
  property groups : Array(Group)

  def initialize(@groups)
  end

  def alive
    groups.any? { |g| g.units > 0 }
  end

  def boost(amount)
    groups.each { |g| g.attack_damage += amount }
  end
end

class Battlefield
  property armies : Hash(Int32, Army)

  def initialize(@armies)
  end

  def find_targets
    armies.each do |army_id, army|
      army.groups.sort_by! { |g| [-g.effective_power, -g.initiative] }
      army.groups.each do |group|
        most_damage = 0
        target_group = nil
        armies.each do |enemy_army_id, enemy_army|
          next if army_id == enemy_army_id
          enemy_army.groups.each do |enemy|
            next if enemy.units <= 0 || enemy.attacker || group.damage_dealt(enemy) == 0
            damage = group.damage_dealt(enemy)
            if damage > most_damage
              most_damage = damage
              target_group = enemy
            elsif damage == most_damage && target_group
              if enemy.effective_power > target_group.effective_power
                target_group = enemy
              elsif enemy.effective_power == target_group.effective_power && enemy.initiative > target_group.initiative
                target_group = enemy
              end
            end
          end
        end
        if target_group
          group.target = target_group
          target_group.attacker = group
        end
      end
    end
  end

  def clean
    armies.each_value { |army| army.groups.select! { |g| g.units > 0 } }
  end

  def active
    armies.values.all?(&.alive)
  end

  def result
    winner = 0
    units = 0
    armies.each do |army_id, army|
      if army.alive
        winner = army_id
        units = army.groups.sum(&.units)
      end
    end
    {winner, units}
  end

  def total_units
    armies.values.sum { |army| army.groups.sum(&.units) }
  end
end

def parse_input(input_data)
  armies = Hash(Int32, Army).new
  initiative = [] of Group
  current_army_id = 0

  army_name_pattern = /^(.*):$/
  group_immunities_pattern = /immune to (.*?)[;)]/
  group_weaknesses_pattern = /weak to (.*?)[;)]/
  group_description_pattern = /^(\d+) units each with (\d+) hit points.*with an attack that does (\d+) (\w+) damage at initiative (\d+)$/

  input_data.lines.each do |line|
    line = line.strip
    if (m = line.match(army_name_pattern))
      army_name = m[1]
      current_army_id = army_name == "Immune System" ? 1 : 2
    elsif (m = line.match(group_description_pattern))
      units = m[1].to_i
      hit_points = m[2].to_i
      attack_damage = m[3].to_i
      attack_type = m[4]
      initiative_value = m[5].to_i

      immunities = (line.match(group_immunities_pattern).try &.[1]?.try &.split(", ")) || [] of String
      weaknesses = (line.match(group_weaknesses_pattern).try &.[1]?.try &.split(", ")) || [] of String

      group = Group.new(units, hit_points, attack_damage, attack_type, initiative_value, immunities, weaknesses)
      armies[current_army_id] ||= Army.new([] of Group)
      armies[current_army_id].groups << group
      initiative << group
    end
  end

  {Battlefield.new(armies), Initiative.new(initiative)}
end

def condition_fight(input_data)
  battle, initiative = parse_input(input_data)
  previous_units = -1
  while battle.active
    battle.find_targets
    initiative.attack
    battle.clean
    initiative.clean
    current_units = battle.total_units
    return 0 if current_units == previous_units
    previous_units = current_units
  end
  battle.result[1]
end

input_data = File.read("input.txt").strip
puts condition_fight(input_data)
