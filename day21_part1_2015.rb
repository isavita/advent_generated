
class Item
  attr_accessor :cost, :damage, :armor

  def initialize(cost, damage = 0, armor = 0)
    @cost = cost
    @damage = damage
    @armor = armor
  end
end

class Character
  attr_accessor :hit_points, :damage, :armor

  def initialize(hit_points, damage, armor)
    @hit_points = hit_points
    @damage = damage
    @armor = armor
  end
end

def parse_stat(line)
  line.split(": ")[1].to_i
end

def player_wins(player, boss)
  player_damage = [1, player.damage - boss.armor].max
  boss_damage = [1, boss.damage - player.armor].max

  player_turns = (boss.hit_points + player_damage - 1) / player_damage
  boss_turns = (player.hit_points + boss_damage - 1) / boss_damage

  player_turns <= boss_turns
end

data = File.read("input.txt").split("\n")
boss = Character.new(parse_stat(data[0]), parse_stat(data[1]), parse_stat(data[2]))

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

min_cost = Float::INFINITY
weapons.each do |w|
  armors.each do |a|
    (0...rings.length).each do |ri|
      ((ri + 1)...rings.length).each do |rj|
        player = Character.new(100, w.damage, a.armor)
        player.damage += rings[ri].damage + rings[rj].damage
        player.armor += rings[ri].armor + rings[rj].armor
        cost = w.cost + a.cost + rings[ri].cost + rings[rj].cost
        min_cost = cost if player_wins(player, boss) && cost < min_cost
      end
    end
  end
end

puts min_cost
