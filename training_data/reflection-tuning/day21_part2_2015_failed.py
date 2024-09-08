def calculate_stats(loadout):
    cost = sum(loadout[::3])  # Every third element starting from index 0
    damage = sum(loadout[1::3])  # Every third element starting from index 1
    armor = sum(loadout[2::3])  # Every third element starting from index 2
    return cost, damage, armor

def simulate_fight(player_hp, player_damage, player_armor, boss_hp, boss_damage, boss_armor):
    while True:
        boss_hp -= max(1, player_damage - boss_armor)
        if boss_hp <= 0:
            return True
        player_hp -= max(1, boss_damage - player_armor)
        if player_hp <= 0:
            return False

def find_optimal_loadout(boss_hp, boss_damage, boss_armor, part2=False):
    weapons = [8, 4, 0, 10, 5, 0, 25, 6, 0, 40, 7, 0, 74, 8, 0]
    armors = [0, 0, 0, 13, 0, 1, 31, 0, 2, 53, 0, 3, 75, 0, 4, 102, 0, 5]
    rings = [0, 0, 0, 0, 0, 0, 25, 1, 0, 50, 2, 0, 100, 3, 0, 20, 0, 1, 40, 0, 2, 80, 0, 3]

    best_cost = float('inf') if not part2 else 0
    
    for weapon in range(0, len(weapons), 3):
        for armor in range(0, len(armors), 3):
            for ring1 in range(0, len(rings), 3):
                for ring2 in range(0, len(rings), 3):
                    if ring1 == ring2 and ring1 != 0:
                        continue
                    
                    loadout = weapons[weapon:weapon+3] + armors[armor:armor+3] + rings[ring1:ring1+3] + rings[ring2:ring2+3]
                    cost, damage, armor = calculate_stats(loadout)
                    
                    player_wins = simulate_fight(100, damage, armor, boss_hp, boss_damage, boss_armor)
                    
                    if (not part2 and player_wins and cost < best_cost) or (part2 and not player_wins and cost > best_cost):
                        best_cost = cost

    return best_cost

# Example usage
boss_hp, boss_damage, boss_armor = 104, 8, 1  # Replace with actual boss stats from puzzle input

print("Part 1:", find_optimal_loadout(boss_hp, boss_damage, boss_armor))
print("Part 2:", find_optimal_loadout(boss_hp, boss_damage, boss_armor, part2=True))
