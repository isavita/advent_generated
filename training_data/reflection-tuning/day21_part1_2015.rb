WEAPONS = [
  { name: 'Dagger',     cost: 8,  damage: 4, armor: 0 },
  { name: 'Shortsword', cost: 10, damage: 5, armor: 0 },
  { name: 'Warhammer',  cost: 25, damage: 6, armor: 0 },
  { name: 'Longsword',  cost: 40, damage: 7, armor: 0 },
  { name: 'Greataxe',   cost: 74, damage: 8, armor: 0 }
]

ARMOR = [
  { name: 'No Armor',   cost: 0,   damage: 0, armor: 0 },
  { name: 'Leather',    cost: 13,  damage: 0, armor: 1 },
  { name: 'Chainmail',  cost: 31,  damage: 0, armor: 2 },
  { name: 'Splintmail', cost: 53,  damage: 0, armor: 3 },
  { name: 'Bandedmail', cost: 75,  damage: 0, armor: 4 },
  { name: 'Platemail',  cost: 102, damage: 0, armor: 5 }
]

RINGS = [
  { name: 'No Ring',    cost: 0,   damage: 0, armor: 0 },
  { name: 'Damage +1',  cost: 25,  damage: 1, armor: 0 },
  { name: 'Damage +2',  cost: 50,  damage: 2, armor: 0 },
  { name: 'Damage +3',  cost: 100, damage: 3, armor: 0 },
  { name: 'Defense +1', cost: 20,  damage: 0, armor: 1 },
  { name: 'Defense +2', cost: 40,  damage: 0, armor: 2 },
  { name: 'Defense +3', cost: 80,  damage: 0, armor: 3 }
]

def simulate_fight(player_hp, player_damage, player_armor, boss_hp, boss_damage, boss_armor)
  while player_hp > 0 && boss_hp > 0
    boss_hp -= [player_damage - boss_armor, 1].max
    return true if boss_hp <= 0
    player_hp -= [boss_damage - player_armor, 1].max
  end
  false
end

def find_min_gold(boss_hp, boss_damage, boss_armor)
  min_gold = Float::INFINITY

  WEAPONS.each do |weapon|
    ARMOR.each do |armor|
      RINGS.combination(2).each do |ring1, ring2|
        cost = weapon[:cost] + armor[:cost] + ring1[:cost] + ring2[:cost]
        next if cost >= min_gold

        player_damage = weapon[:damage] + ring1[:damage] + ring2[:damage]
        player_armor = armor[:armor] + ring1[:armor] + ring2[:armor]

        if simulate_fight(100, player_damage, player_armor, boss_hp, boss_damage, boss_armor)
          min_gold = cost if cost < min_gold
        end
      end
    end
  end

  min_gold
end

# Read boss stats from input
boss_hp, boss_damage, boss_armor = File.readlines('input.txt').map { |line| line.split(': ').last.to_i }

puts find_min_gold(boss_hp, boss_damage, boss_armor)
