WEAPONS = [
  { name: 'Dagger',     cost: 8,  damage: 4, armor: 0 },
  { name: 'Shortsword', cost: 10, damage: 5, armor: 0 },
  { name: 'Warhammer',  cost: 25, damage: 6, armor: 0 },
  { name: 'Longsword',  cost: 40, damage: 7, armor: 0 },
  { name: 'Greataxe',   cost: 74, damage: 8, armor: 0 }
]

ARMOR = [
  { name: 'Leather',    cost: 13,  damage: 0, armor: 1 },
  { name: 'Chainmail',  cost: 31,  damage: 0, armor: 2 },
  { name: 'Splintmail', cost: 53,  damage: 0, armor: 3 },
  { name: 'Bandedmail', cost: 75,  damage: 0, armor: 4 },
  { name: 'Platemail',  cost: 102, damage: 0, armor: 5 }
]

RINGS = [
  { name: 'Damage +1',  cost: 25,  damage: 1, armor: 0 },
  { name: 'Damage +2',  cost: 50,  damage: 2, armor: 0 },
  { name: 'Damage +3',  cost: 100, damage: 3, armor: 0 },
  { name: 'Defense +1', cost: 20,  damage: 0, armor: 1 },
  { name: 'Defense +2', cost: 40,  damage: 0, armor: 2 },
  { name: 'Defense +3', cost: 80,  damage: 0, armor: 3 }
]

def generate_combinations(weapons, armors, rings)
  weapon_combinations = weapons.map { |w| [w] }
  armor_combinations = [[]] + armors.map { |a| [a] }
  ring_combinations = [[]] + rings.combination(1).to_a + rings.combination(2).to_a

  weapon_combinations.product(armor_combinations, ring_combinations).map(&:flatten)
end

def calculate_stats(items)
  {
    cost: items.sum { |item| item[:cost] },
    damage: items.sum { |item| item[:damage] },
    armor: items.sum { |item| item[:armor] }
  }
end

def player_wins?(player_hp, player_damage, player_armor, boss_hp, boss_damage, boss_armor)
  player_damage_dealt = [player_damage - boss_armor, 1].max
  boss_damage_dealt = [boss_damage - player_armor, 1].max

  turns_to_kill_boss = (boss_hp.to_f / player_damage_dealt).ceil
  turns_to_kill_player = (player_hp.to_f / boss_damage_dealt).ceil

  turns_to_kill_boss <= turns_to_kill_player
end

def solve(boss_hp, boss_damage, boss_armor)
  combinations = generate_combinations(WEAPONS, ARMOR, RINGS)

  min_cost_to_win = Float::INFINITY
  max_cost_to_lose = 0

  combinations.each do |combo|
    stats = calculate_stats(combo)
    if player_wins?(100, stats[:damage], stats[:armor], boss_hp, boss_damage, boss_armor)
      min_cost_to_win = [min_cost_to_win, stats[:cost]].min
    else
      max_cost_to_lose = [max_cost_to_lose, stats[:cost]].max
    end
  end

  [min_cost_to_win, max_cost_to_lose]
end

boss_hp, boss_damage, boss_armor = 104, 8, 1
min_cost, max_cost = solve(boss_hp, boss_damage, boss_armor)

puts "Part 1: The least amount of gold to spend and still win is #{min_cost}"
puts "Part 2: The most amount of gold to spend and still lose is #{max_cost}"
