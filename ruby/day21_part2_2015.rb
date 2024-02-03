
# Read input from file
input = File.read('input.txt').split("\n")

# Boss stats
boss_hp = input[0].split(': ')[1].to_i
boss_damage = input[1].split(': ')[1].to_i
boss_armor = input[2].split(': ')[1].to_i

# Player stats
player_hp = 100
player_damage = 0
player_armor = 0

# Item shop
weapons = [
  { name: 'Dagger', cost: 8, damage: 4, armor: 0 },
  { name: 'Shortsword', cost: 10, damage: 5, armor: 0 },
  { name: 'Warhammer', cost: 25, damage: 6, armor: 0 },
  { name: 'Longsword', cost: 40, damage: 7, armor: 0 },
  { name: 'Greataxe', cost: 74, damage: 8, armor: 0 }
]

armor = [
  { name: 'Leather', cost: 13, damage: 0, armor: 1 },
  { name: 'Chainmail', cost: 31, damage: 0, armor: 2 },
  { name: 'Splintmail', cost: 53, damage: 0, armor: 3 },
  { name: 'Bandedmail', cost: 75, damage: 0, armor: 4 },
  { name: 'Platemail', cost: 102, damage: 0, armor: 5 }
]

rings = [
  { name: 'Damage +1', cost: 25, damage: 1, armor: 0 },
  { name: 'Damage +2', cost: 50, damage: 2, armor: 0 },
  { name: 'Damage +3', cost: 100, damage: 3, armor: 0 },
  { name: 'Defense +1', cost: 20, damage: 0, armor: 1 },
  { name: 'Defense +2', cost: 40, damage: 0, armor: 2 },
  { name: 'Defense +3', cost: 80, damage: 0, armor: 3 }
]

# Initialize variables
min_gold = Float::INFINITY
max_gold = 0

# Iterate through all combinations of items
weapons.each do |weapon|
  armor_combinations = armor + [nil]
  rings_combinations = rings + [nil]

  armor_combinations.each do |armor|
    rings_combinations.combination(2).each do |ring1, ring2|
      total_cost = weapon[:cost] + (armor.nil? ? 0 : armor[:cost]) + (ring1.nil? ? 0 : ring1[:cost]) + (ring2.nil? ? 0 : ring2[:cost])
      total_damage = weapon[:damage] + (armor.nil? ? 0 : armor[:damage]) + (ring1.nil? ? 0 : ring1[:damage]) + (ring2.nil? ? 0 : ring2[:damage])
      total_armor = weapon[:armor] + (armor.nil? ? 0 : armor[:armor]) + (ring1.nil? ? 0 : ring1[:armor]) + (ring2.nil? ? 0 : ring2[:armor])

      player_hp_needed = (boss_hp.to_f / [1, (total_damage - boss_armor)].max).ceil
      boss_hp_needed = (player_hp.to_f / [1, (boss_damage - total_armor)].max).ceil

      if player_hp_needed <= boss_hp_needed
        min_gold = [min_gold, total_cost].min
      else
        max_gold = [max_gold, total_cost].max
      end
    end
  end
end

puts min_gold
puts max_gold
