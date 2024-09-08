# Read boss stats from input file
boss_hp, boss_damage, boss_armor = File.readlines('input.txt').map { |line| line.split(': ')[1].to_i }

WEAPONS = [
  [8, 4, 0], [10, 5, 0], [25, 6, 0], [40, 7, 0], [74, 8, 0]
]
ARMOR = [
  [0, 0, 0], [13, 0, 1], [31, 0, 2], [53, 0, 3], [75, 0, 4], [102, 0, 5]
]
RINGS = [
  [0, 0, 0], [0, 0, 0], [25, 1, 0], [50, 2, 0], [100, 3, 0],
  [20, 0, 1], [40, 0, 2], [80, 0, 3]
]

def simulate_fight(player_hp, player_damage, player_armor, boss_hp, boss_damage, boss_armor)
  player_damage_dealt = [player_damage - boss_armor, 1].max
  boss_damage_dealt = [boss_damage - player_armor, 1].max
  turns_to_kill_boss = (boss_hp.to_f / player_damage_dealt).ceil
  turns_to_die = (player_hp.to_f / boss_damage_dealt).ceil
  turns_to_kill_boss <= turns_to_die
end

def find_min_gold(weapon_index, armor_index, ring1_index, ring2_index, min_gold)
  return min_gold if weapon_index == WEAPONS.size

  weapon = WEAPONS[weapon_index]
  armor = ARMOR[armor_index]
  ring1 = RINGS[ring1_index]
  ring2 = RINGS[ring2_index]

  total_cost = weapon[0] + armor[0] + ring1[0] + ring2[0]
  return find_min_gold(weapon_index + 1, 0, 0, 1, min_gold) if total_cost >= min_gold

  player_damage = weapon[1] + armor[1] + ring1[1] + ring2[1]
  player_armor = weapon[2] + armor[2] + ring1[2] + ring2[2]

  if simulate_fight(100, player_damage, player_armor, boss_hp, boss_damage, boss_armor)
    min_gold = total_cost
  end

  min_gold = find_min_gold(weapon_index, armor_index, ring1_index, ring2_index + 1, min_gold) if ring2_index < RINGS.size - 1
  min_gold = find_min_gold(weapon_index, armor_index, ring1_index + 1, ring1_index + 2, min_gold) if ring1_index < RINGS.size - 2
  min_gold = find_min_gold(weapon_index, armor_index + 1, 0, 1, min_gold) if armor_index < ARMOR.size - 1
  min_gold = find_min_gold(weapon_index + 1, 0, 0, 1, min_gold)

  min_gold
end

puts find_min_gold(0, 0, 0, 1, Float::INFINITY)
