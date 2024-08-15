require "file_utils"

struct Item
  property cost
  property damage
  property armor

  def initialize(@cost : Int32, @damage : Int32 = 0, @armor : Int32 = 0)
  end
end

struct Character
  property hit_points
  property damage
  property armor

  def initialize(@hit_points : Int32, @damage : Int32, @armor : Int32)
  end
end

def parse_stat(line : String) : Int32
  line.split(": ")[1].to_i
end

def player_wins?(player : Character, boss : Character) : Bool
  player_damage = [1, player.damage - boss.armor].max
  boss_damage = [1, boss.damage - player.armor].max

  player_turns = (boss.hit_points + player_damage - 1) // player_damage
  boss_turns = (player.hit_points + boss_damage - 1) // boss_damage

  player_turns <= boss_turns
end

weapons = [
  Item.new(8, 4),
  Item.new(10, 5),
  Item.new(25, 6),
  Item.new(40, 7),
  Item.new(74, 8)
]

armors = [
  Item.new(0),
  Item.new(13, 0, 1),
  Item.new(31, 0, 2),
  Item.new(53, 0, 3),
  Item.new(75, 0, 4),
  Item.new(102, 0, 5)
]

rings = [
  Item.new(0),
  Item.new(25, 1),
  Item.new(50, 2),
  Item.new(100, 3),
  Item.new(20, 0, 1),
  Item.new(40, 0, 2),
  Item.new(80, 0, 3)
]

input = File.read("input.txt").split("\n")
boss = Character.new(parse_stat(input[0]), parse_stat(input[1]), parse_stat(input[2]))

min_cost = Int32::MAX
weapons.each do |weapon|
  armors.each do |armor|
    rings.each_with_index do |ring1, i|
      (i + 1...rings.size).each do |j|
        ring2 = rings[j]
        player = Character.new(100, weapon.damage + ring1.damage + ring2.damage, armor.armor + ring1.armor + ring2.armor)
        cost = weapon.cost + armor.cost + ring1.cost + ring2.cost
        if player_wins?(player, boss) && cost < min_cost
          min_cost = cost
        end
      end
    end
  end
end

puts min_cost