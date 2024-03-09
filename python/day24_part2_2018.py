import re
from typing import List, Dict, Tuple

class Group:
    def __init__(self, units: int, hit_points: int, attack_damage: int, attack_type: str, initiative: int, immunities: List[str], weaknesses: List[str]):
        self.units = units
        self.hit_points = hit_points
        self.attack_damage = attack_damage
        self.attack_type = attack_type
        self.initiative = initiative
        self.immunities = immunities
        self.weaknesses = weaknesses
        self.attacker = None
        self.target = None

    def effective_power(self) -> int:
        return self.units * self.attack_damage

    def damage_dealt(self, enemy: 'Group') -> int:
        if self.attack_type in enemy.immunities:
            return 0
        if self.attack_type in enemy.weaknesses:
            return self.effective_power() * 2
        return self.effective_power()

    def __str__(self) -> str:
        out = f"{self.units} units each with {self.hit_points} hit points"
        if self.immunities or self.weaknesses:
            out += " ("
            if self.immunities:
                out += "immune to " + " and ".join(self.immunities)
                if self.weaknesses:
                    out += "; "
            if self.weaknesses:
                out += "weak to " + " and ".join(self.weaknesses)
            out += ")"
        out += f" with an attack that does {self.attack_damage} {self.attack_type} damage at initiative {self.initiative}"
        return out

class Initiative:
    def __init__(self, groups: List[Group]):
        self.groups = groups

    def attack(self) -> None:
        self.groups.sort(key=lambda g: g.initiative, reverse=True)
        for group in self.groups:
            if group.units > 0 and group.target and group.target.units > 0:
                group.target.units -= group.damage_dealt(group.target) // group.target.hit_points
            if group.target:
                group.target.attacker = None
                group.target = None

    def clean(self) -> None:
        self.groups = [g for g in self.groups if g.units > 0]
        self.groups.sort(key=lambda g: g.initiative, reverse=True)

class Army:
    def __init__(self, groups: List[Group]):
        self.groups = groups

    def alive(self) -> bool:
        return any(g.units > 0 for g in self.groups)

    def boost(self, amount: int) -> None:
        for g in self.groups:
            g.attack_damage += amount

class Battlefield:
    def __init__(self, armies: Dict[int, Army]):
        self.armies = armies

    def find_targets(self) -> None:
        for army_id, army in self.armies.items():
            army.groups.sort(key=lambda g: (g.effective_power(), g.initiative), reverse=True)
            for group in army.groups:
                for enemy_army_id, enemy_army in self.armies.items():
                    if army_id == enemy_army_id or group.units <= 0:
                        continue
                    most_damage = 0
                    target_group = None
                    for enemy_group in enemy_army.groups:
                        if enemy_group.units <= 0 or enemy_group.attacker or group.damage_dealt(enemy_group) == 0 or group.damage_dealt(enemy_group) < most_damage:
                            continue
                        if group.damage_dealt(enemy_group) == most_damage and target_group:
                            if enemy_group.effective_power() < target_group.effective_power():
                                continue
                            if enemy_group.effective_power() == target_group.effective_power() and enemy_group.initiative < target_group.initiative:
                                continue
                        most_damage = group.damage_dealt(enemy_group)
                        target_group = enemy_group
                    if target_group:
                        group.target = target_group
                        target_group.attacker = group

    def clean(self) -> None:
        for army_id in self.armies:
            self.armies[army_id].groups = [g for g in self.armies[army_id].groups if g.units > 0]

    def active(self) -> bool:
        return all(army.alive() for army in self.armies.values())

    def result(self) -> Tuple[int, int]:
        winner = 0
        units = 0
        for army_id, army in self.armies.items():
            if army.alive():
                winner = army_id
                units = sum(g.units for g in army.groups if g.units > 0)
        return winner, units

    def total_units(self) -> int:
        return sum(g.units for army in self.armies.values() for g in army.groups if g.units > 0)

def parse_input(input_data: str) -> Tuple[Battlefield, Initiative]:
    armies = {}
    initiative = []
    current_army_id = 0

    army_name_pattern = re.compile(r'^(.*):$')
    group_immunities_pattern = re.compile(r'immune to (.*?)[;)]')
    group_weaknesses_pattern = re.compile(r'weak to (.*?)[;)]')
    group_description_pattern = re.compile(r'^(\d+) units each with (\d+) hit points.*with an attack that does (\d+) (\w+) damage at initiative (\d+)$')

    for line in input_data.split('\n'):
        army_name_match = army_name_pattern.match(line)
        if army_name_match:
            army_name = army_name_match.group(1)
            if army_name == "Immune System":
                current_army_id = 1
            elif army_name == "Infection":
                current_army_id = 2
            else:
                raise ValueError(f"Unknown army: {army_name}")
        else:
            if current_army_id <= 0 or current_army_id >= 3:
                raise ValueError(f"Tried to assign group to invalid army: {current_army_id}")
            group_description_match = group_description_pattern.match(line)
            if not group_description_match:
                continue
            units = int(group_description_match.group(1))
            hit_points = int(group_description_match.group(2))
            attack_damage = int(group_description_match.group(3))
            attack_type = group_description_match.group(4)
            initiative_value = int(group_description_match.group(5))
            immunities_match = group_immunities_pattern.search(line)
            immunities = immunities_match.group(1).split(", ") if immunities_match else []
            weaknesses_match = group_weaknesses_pattern.search(line)
            weaknesses = weaknesses_match.group(1).split(", ") if weaknesses_match else []

            group = Group(units, hit_points, attack_damage, attack_type, initiative_value, immunities, weaknesses)
            if current_army_id not in armies:
                armies[current_army_id] = Army([])
            armies[current_army_id].groups.append(group)
            initiative.append(group)

    return Battlefield(armies), Initiative(initiative)

def immune_system_boost(input_data: str) -> int:
    boost = 0
    while True:
        stalemate = False
        battle, initiative = parse_input(input_data)

        battle.armies[1].boost(boost)

        while battle.active():
            before = battle.total_units()

            battle.find_targets()
            initiative.attack()

            if battle.total_units() == before:
                stalemate = True
                break

            battle.clean()
            initiative.clean()

        if not stalemate:
            winner, units = battle.result()
            if winner == 1:
                return units

        boost += 1

def main():
    with open("input.txt", "r") as file:
        input_data = file.read().strip()
    print(immune_system_boost(input_data))

if __name__ == "__main__":
    main()
