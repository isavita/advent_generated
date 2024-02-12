
struct Item
  property cost : Int32
  property damage : Int32
  property armor : Int32

  def initialize(@cost : Int32, @damage : Int32, @armor : Int32 = 0)
  end
end

struct Character
  property hit_points : Int32
  property damage : Int32
  property armor : Int32

  def initialize(@hit_points : Int32, @damage : Int32, @armor : Int32)
  end
end

def parse_stat(line : String) : Int32
  line.split(": ")[1].to_i
end

def player_wins(player : Character, boss : Character) : Bool
  player_damage = player.damage > boss.armor ? player.damage - boss.armor : 1
  boss_damage = boss.damage > player.armor ? boss.damage - player.armor : 1

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
  Item.new(0, 0),
  Item.new(13, 0, 1),
  Item.new(31, 0, 2),
  Item.new(53, 0, 3),
  Item.new(75, 0, 4),
  Item.new(102, 0, 5)
]

rings = [
  Item.new(0, 0, 0),
  Item.new(25, 1),
  Item.new(50, 2),
  Item.new(100, 3),
  Item.new(20, 0, 1),
  Item.new(40, 0, 2),
  Item.new(80, 0, 3)
]

max_cost = 0
weapons.each do |w|
  armors.each do |a|
    (0...rings.size).each do |ri|
      ((ri + 1)...rings.size).each do |rj|
        player = Character.new(100, w.damage, a.armor)
        player.damage += rings[ri].damage + rings[rj].damage
        player.armor += rings[ri].armor + rings[rj].armor
        cost = w.cost + a.cost + rings[ri].cost + rings[rj].cost
        max_cost = cost if !player_wins(player, boss) && cost > max_cost
      end
    end
  end
end

puts max_cost
