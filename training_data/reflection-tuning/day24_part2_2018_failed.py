import re
from copy import deepcopy

class Group:
    def __init__(self, unit_count, hit_points, attack_damage, attack_type, initiative, weaknesses, immunities):
        self.unit_count = unit_count
        self.hit_points = hit_points
        self.attack_damage = attack_damage
        self.attack_type = attack_type
        self.initiative = initiative
        self.weaknesses = weaknesses
        self.immunities = immunities

    @property
    def effective_power(self):
        return self.unit_count * self.attack_damage

def parse_input(input_str):
    immune_system, infection = input_str.split('\n\n')
    return [parse_army(immune_system), parse_army(infection)]

def parse_army(army_str):
    groups = []
    for line in army_str.split('\n')[1:]:
        if line:
            match = re.match(r'(\d+) units each with (\d+) hit points (\([^)]*\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)', line)
            unit_count, hit_points, modifiers, attack_damage, attack_type, initiative = match.groups()
            weaknesses, immunities = parse_modifiers(modifiers)
            groups.append(Group(int(unit_count), int(hit_points), int(attack_damage), attack_type, int(initiative), weaknesses, immunities))
    return groups

def parse_modifiers(modifiers):
    weaknesses, immunities = [], []
    if modifiers:
        for modifier in modifiers.strip('()').split(';'):
            if 'weak to' in modifier:
                weaknesses = modifier.split('weak to ')[1].split(', ')
            elif 'immune to' in modifier:
                immunities = modifier.split('immune to ')[1].split(', ')
    return weaknesses, immunities

def calculate_damage(attacker, defender):
    if attacker.attack_type in defender.immunities:
        return 0
    damage = attacker.effective_power
    if attacker.attack_type in defender.weaknesses:
        damage *= 2
    return damage

def select_targets(attackers, defenders):
    targets = {}
    available_defenders = defenders.copy()
    for attacker in sorted(attackers, key=lambda x: (x.effective_power, x.initiative), reverse=True):
        best_target = max(available_defenders, key=lambda x: (calculate_damage(attacker, x), x.effective_power, x.initiative), default=None)
        if best_target and calculate_damage(attacker, best_target) > 0:
            targets[attacker] = best_target
            available_defenders.remove(best_target)
    return targets

def attack(attacker, defender):
    damage = calculate_damage(attacker, defender)
    units_killed = min(damage // defender.hit_points, defender.unit_count)
    defender.unit_count -= units_killed
    return units_killed > 0

def battle(immune_system, infection):
    while immune_system and infection:
        targets_immune = select_targets(immune_system, infection)
        targets_infection = select_targets(infection, immune_system)
        
        all_groups = sorted(immune_system + infection, key=lambda x: x.initiative, reverse=True)
        units_killed = 0
        for group in all_groups:
            if group.unit_count > 0:
                if group in targets_immune:
                    units_killed += attack(group, targets_immune[group])
                elif group in targets_infection:
                    units_killed += attack(group, targets_infection[group])
        
        immune_system = [group for group in immune_system if group.unit_count > 0]
        infection = [group for group in infection if group.unit_count > 0]
        
        if units_killed == 0:
            return None, immune_system + infection  # Stalemate
    
    return "Immune System" if immune_system else "Infection", immune_system + infection

def simulate_with_boost(immune_system, infection, boost):
    boosted_immune = deepcopy(immune_system)
    for group in boosted_immune:
        group.attack_damage += boost
    winner, remaining = battle(boosted_immune, deepcopy(infection))
    return winner, sum(group.unit_count for group in remaining)

def find_minimum_boost(immune_system, infection):
    low, high = 0, 10000
    while low < high:
        mid = (low + high) // 2
        winner, units = simulate_with_boost(immune_system, infection, mid)
        if winner == "Immune System":
            high = mid
        else:
            low = mid + 1
    return low, simulate_with_boost(immune_system, infection, low)[1]

def solve(input_str):
    immune_system, infection = parse_input(input_str)
    
    # Part 1
    winner, remaining = battle(deepcopy(immune_system), deepcopy(infection))
    part1 = sum(group.unit_count for group in remaining)
    
    # Part 2
    boost, part2 = find_minimum_boost(immune_system, infection)
    
    return part1, part2

# Example usage:
input_str = """Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4"""

part1, part2 = solve(input_str)
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
