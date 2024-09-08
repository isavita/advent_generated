import re
from typing import List, Tuple, Dict

class Group:
    def __init__(self, army: str, units: int, hp: int, immunities: List[str], weaknesses: List[str], 
                 damage: int, attack_type: str, initiative: int):
        self.army = army
        self.units = units
        self.hp = hp
        self.immunities = immunities
        self.weaknesses = weaknesses
        self.damage = damage
        self.attack_type = attack_type
        self.initiative = initiative

    @property
    def effective_power(self) -> int:
        return self.units * self.damage

    def calculate_damage(self, target: 'Group') -> int:
        if self.attack_type in target.immunities:
            return 0
        elif self.attack_type in target.weaknesses:
            return self.effective_power * 2
        else:
            return self.effective_power

    def take_damage(self, damage: int) -> None:
        units_lost = min(damage // self.hp, self.units)
        self.units -= units_lost

def parse_input(input_data: str) -> Tuple[List[Group], List[Group]]:
    immune_system, infection = input_data.split('\n\n')
    return (parse_army(immune_system, 'Immune System'), parse_army(infection, 'Infection'))

def parse_army(army_data: str, army_name: str) -> List[Group]:
    groups = []
    for line in army_data.split('\n')[1:]:  # Skip the first line (army name)
        match = re.match(r'(\d+) units each with (\d+) hit points (\([^)]*\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)', line)
        if match:
            units, hp, modifiers, damage, attack_type, initiative = match.groups()
            immunities, weaknesses = [], []
            if modifiers:
                for modifier in modifiers.strip('()').split('; '):
                    if modifier.startswith('weak to'):
                        weaknesses = modifier[8:].split(', ')
                    elif modifier.startswith('immune to'):
                        immunities = modifier[10:].split(', ')
            groups.append(Group(army_name, int(units), int(hp), immunities, weaknesses, int(damage), attack_type, int(initiative)))
    return groups

def select_targets(attacking: List[Group], defending: List[Group]) -> Dict[Group, Group]:
    targets = {}
    available_targets = set(defending)
    for attacker in sorted(attacking, key=lambda g: (g.effective_power, g.initiative), reverse=True):
        if not available_targets:
            break
        best_target = max(available_targets, key=lambda t: (attacker.calculate_damage(t), t.effective_power, t.initiative))
        if attacker.calculate_damage(best_target) > 0:
            targets[attacker] = best_target
            available_targets.remove(best_target)
    return targets

def combat(immune_system: List[Group], infection: List[Group]) -> int:
    while immune_system and infection:
        targets = {**select_targets(immune_system, infection), **select_targets(infection, immune_system)}
        if not targets:
            break  # Stalemate
        for attacker in sorted(targets.keys(), key=lambda g: g.initiative, reverse=True):
            if attacker.units <= 0:
                continue
            defender = targets[attacker]
            damage = attacker.calculate_damage(defender)
            defender.take_damage(damage)
        immune_system = [g for g in immune_system if g.units > 0]
        infection = [g for g in infection if g.units > 0]
    
    winning_army = immune_system or infection
    return sum(group.units for group in winning_army)

def solve(input_data: str) -> int:
    immune_system, infection = parse_input(input_data)
    return combat(immune_system, infection)

# Example usage:
input_data = """Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4"""

result = solve(input_data)
print(result)  # This should print the correct number of units in the winning army
