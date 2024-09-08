import itertools

def parse_input(filename):
    with open(filename, 'r') as f:
        lines = f.readlines()
    boss_hp = int(lines[0].split(': ')[1])
    boss_damage = int(lines[1].split(': ')[1])
    boss_armor = int(lines[2].split(': ')[1])
    return boss_hp, boss_damage, boss_armor

def player_wins(player_hp, player_damage, player_armor, boss_hp, boss_damage, boss_armor):
    while True:
        boss_hp -= max(1, player_damage - boss_armor)
        if boss_hp <= 0:
            return True
        player_hp -= max(1, boss_damage - player_armor)
        if player_hp <= 0:
            return False

def solve(boss_hp, boss_damage, boss_armor):
    weapons = [(8, 4, 0), (10, 5, 0), (25, 6, 0), (40, 7, 0), (74, 8, 0)]
    armors = [(0, 0, 0), (13, 0, 1), (31, 0, 2), (53, 0, 3), (75, 0, 4), (102, 0, 5)]
    rings = [(0, 0, 0), (0, 0, 0), (25, 1, 0), (50, 2, 0), (100, 3, 0), (20, 0, 1), (40, 0, 2), (80, 0, 3)]

    min_cost = float('inf')
    for weapon in weapons:
        for armor in armors:
            for ring1, ring2 in itertools.combinations(rings, 2):
                cost = weapon[0] + armor[0] + ring1[0] + ring2[0]
                damage = weapon[1] + armor[1] + ring1[1] + ring2[1]
                defense = weapon[2] + armor[2] + ring1[2] + ring2[2]
                
                if player_wins(100, damage, defense, boss_hp, boss_damage, boss_armor):
                    min_cost = min(min_cost, cost)

    return min_cost

boss_hp, boss_damage, boss_armor = parse_input("input.txt")
result = solve(boss_hp, boss_damage, boss_armor)
print(result)
