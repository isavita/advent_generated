
from itertools import combinations

weapons = [
    ("Dagger", 8, 4, 0),
    ("Shortsword", 10, 5, 0),
    ("Warhammer", 25, 6, 0),
    ("Longsword", 40, 7, 0),
    ("Greataxe", 74, 8, 0)
]

armor = [
    ("None", 0, 0, 0),
    ("Leather", 13, 0, 1),
    ("Chainmail", 31, 0, 2),
    ("Splintmail", 53, 0, 3),
    ("Bandedmail", 75, 0, 4),
    ("Platemail", 102, 0, 5)
]

rings = [
    ("None", 0, 0, 0),
    ("Damage +1", 25, 1, 0),
    ("Damage +2", 50, 2, 0),
    ("Damage +3", 100, 3, 0),
    ("Defense +1", 20, 0, 1),
    ("Defense +2", 40, 0, 2),
    ("Defense +3", 80, 0, 3)
]

boss_stats = (103, 9, 2)  # Hit points, damage, armor

player_stats = (100, 0, 0)  # Hit points, damage, armor

min_gold = float('inf')

for weapon in weapons:
    for armor_choice in armor:
        for ring1, ring2 in combinations(rings, 2):
            cost = weapon[1] + armor_choice[1] + ring1[1] + ring2[1]
            player_damage = weapon[2] + armor_choice[2] + ring1[2] + ring2[2]
            player_armor = weapon[3] + armor_choice[3] + ring1[3] + ring2[3]

            player_turns = -(-boss_stats[0] // max(1, player_damage - boss_stats[2]))
            boss_turns = -(-player_stats[0] // max(1, boss_stats[1] - player_armor))

            if player_turns <= boss_turns:
                min_gold = min(min_gold, cost)

print(min_gold)
