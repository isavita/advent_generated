
import 'dart:io';
import 'dart:math';

enum ArmyType { immuneSystem, infection }

class Group {
  int units;
  final int hp;
  final int attackDamage;
  final String attackType;
  final int initiative;
  final Set<String> weaknesses;
  final Set<String> immunities;
  final ArmyType armyType;
  int boostedAttackDamage;

  Group({
    required this.units,
    required this.hp,
    required this.attackDamage,
    required this.attackType,
    required this.initiative,
    required this.weaknesses,
    required this.immunities,
    required this.armyType,
    this.boostedAttackDamage = 0,
  });

  int get effectivePower => units * (attackDamage + boostedAttackDamage);

  int calculateDamageTo(Group defender) {
    if (defender.immunities.contains(attackType)) {
      return 0;
    }
    int damage = effectivePower;
    if (defender.weaknesses.contains(attackType)) {
      damage *= 2;
    }
    return damage;
  }

  void receiveDamage(int damage) {
    if (units <= 0) return;
    int unitsLost = damage ~/ hp;
    units = max(0, units - unitsLost);
  }

  Group copyWithUnits(int newUnits) {
    return Group(
      units: newUnits,
      hp: hp,
      attackDamage: attackDamage,
      attackType: attackType,
      initiative: initiative,
      weaknesses: weaknesses,
      immunities: immunities,
      armyType: armyType,
      boostedAttackDamage: boostedAttackDamage,
    );
  }

  @override
  String toString() {
    return '$armyType group with $units units, ep: $effectivePower, init: $initiative';
  }
}

(List<Group>, List<Group>) parseInput(String input) {
  final immuneSystemGroups = <Group>[];
  final infectionGroups = <Group>[];
  ArmyType currentArmy = ArmyType.immuneSystem;

  final lines = input.trim().split('\n');
  for (final line in lines) {
    if (line == 'Immune System:') {
      currentArmy = ArmyType.immuneSystem;
    } else if (line == 'Infection:') {
      currentArmy = ArmyType.infection;
    } else if (line.isNotEmpty) {
      final parts = line.split(' ');
      final units = int.parse(parts[0]);
      final hp = int.parse(parts[4]);

      Set<String> weaknesses = {};
      Set<String> immunities = {};

      if (line.contains('(')) {
        final effectsPart = line.substring(line.indexOf('(') + 1, line.indexOf(')'));
        final effects = effectsPart.split('; ');
        for (final effect in effects) {
          if (effect.startsWith('weak to ')) {
            weaknesses = effect.substring('weak to '.length).split(', ').toSet();
          } else if (effect.startsWith('immune to ')) {
            immunities = effect.substring('immune to '.length).split(', ').toSet();
          }
        }
      }

      final attackDamage = int.parse(parts[parts.indexOf('does') + 1]);
      final attackType = parts[parts.indexOf('does') + 2];
      final initiative = int.parse(parts.last);

      final group = Group(
        units: units,
        hp: hp,
        attackDamage: attackDamage,
        attackType: attackType,
        initiative: initiative,
        weaknesses: weaknesses,
        immunities: immunities,
        armyType: currentArmy,
      );
      if (currentArmy == ArmyType.immuneSystem) {
        immuneSystemGroups.add(group);
      } else {
        infectionGroups.add(group);
      }
    }
  }
  return (immuneSystemGroups, infectionGroups);
}

Map<Group, Group?> targetSelectionPhase(
    List<Group> attackers, List<Group> defenders) {
  final targets = <Group, Group?>{};
  final defenderTargets = <Group>{};

  final sortedAttackers = [...attackers]..sort((a, b) {
    final powerDiff = b.effectivePower - a.effectivePower;
    if (powerDiff != 0) {
      return powerDiff;
    }
    return b.initiative - a.initiative;
  });

  for (final attacker in sortedAttackers) {
    if (attacker.units <= 0) continue;

    Group? bestTarget;
    int maxDamage = -1;

    final potentialTargets = defenders.where((defender) =>
        defender.units > 0 && !defenderTargets.contains(defender));

    for (final defender in potentialTargets) {
      final damage = attacker.calculateDamageTo(defender);
      if (damage > maxDamage) {
        maxDamage = damage;
        bestTarget = defender;
      } else if (damage == maxDamage && bestTarget != null) {
        if (defender.effectivePower > bestTarget.effectivePower) {
          bestTarget = defender;
        } else if (defender.effectivePower == bestTarget.effectivePower &&
            defender.initiative > bestTarget.initiative) {
          bestTarget = defender;
        }
      }
    }

    if (bestTarget != null && maxDamage > 0) {
      targets[attacker] = bestTarget;
      defenderTargets.add(bestTarget);
    } else {
      targets[attacker] = null;
    }
  }
  return targets;
}

bool attackingPhase(Map<Group, Group?> targets) {
  if (targets.isEmpty) return false;

  final attackers = targets.keys.toList()..sort((a, b) => b.initiative - a.initiative);
  bool damageDealt = false;

  for (final attacker in attackers) {
    if (attacker.units <= 0) continue;
    final defender = targets[attacker];
    if (defender != null && defender.units > 0) {
      final damage = attacker.calculateDamageTo(defender);
      final initialUnits = defender.units;
      defender.receiveDamage(damage);
      if (defender.units < initialUnits) {
        damageDealt = true;
      }
    }
  }
  return damageDealt;
}

ArmyType? simulateCombat(List<Group> immuneSystem, List<Group> infection) {
  while (immuneSystem.isNotEmpty && infection.isNotEmpty) {
    final targetMapImmune = targetSelectionPhase(immuneSystem, infection);
    final targetMapInfection = targetSelectionPhase(infection, immuneSystem);

    final allTargets = <Group, Group?>{...targetMapImmune, ...targetMapInfection};

    bool damageDealt = attackingPhase(allTargets);
    if (!damageDealt) return null; // Stalemate

    immuneSystem.removeWhere((group) => group.units <= 0);
    infection.removeWhere((group) => group.units <= 0);
  }

  if (immuneSystem.isNotEmpty) {
    return ArmyType.immuneSystem;
  } else if (infection.isNotEmpty) {
    return ArmyType.infection;
  } else {
    return null; // Should not happen as per problem statement, but for completeness.
  }
}

int solvePart1(String input) {
  final (initialImmuneSystem, initialInfection) = parseInput(input);
  final immuneSystem = initialImmuneSystem.map((g) => g.copyWithUnits(g.units)).toList();
  final infection = initialInfection.map((g) => g.copyWithUnits(g.units)).toList();

  final winner = simulateCombat(immuneSystem, infection);

  if (winner == ArmyType.immuneSystem) {
    return immuneSystem.fold(0, (sum, group) => sum + group.units);
  } else if (winner == ArmyType.infection) {
    return infection.fold(0, (sum, group) => sum + group.units);
  } else {
    return 0; // Stalemate
  }
}

int solvePart2(String input) {
  final (initialImmuneSystem, initialInfection) = parseInput(input);

  int boost = 0;
  while (true) {
    boost++;
    final immuneSystem = initialImmuneSystem.map((g) => Group(
        units: g.units,
        hp: g.hp,
        attackDamage: g.attackDamage,
        attackType: g.attackType,
        initiative: g.initiative,
        weaknesses: g.weaknesses,
        immunities: g.immunities,
        armyType: g.armyType,
        boostedAttackDamage: boost)).toList();
    final infection = initialInfection.map((g) => g.copyWithUnits(g.units)).toList();

    final winner = simulateCombat(immuneSystem, infection);

    if (winner == ArmyType.immuneSystem) {
      if (simulateCombat(immuneSystem, infection) == ArmyType.immuneSystem) {
          return immuneSystem.fold(0, (sum, group) => sum + group.units);
      }
    } else if (winner == null) {
        continue; // Stalemate, try higher boost
    }
  }
}


void main() {
  final inputFile = File('input.txt');
  final input = inputFile.readAsStringSync();

  final part1Result = solvePart1(input);
  print(part1Result); // Part 1 Answer

  final part2Result = solvePart2(input);
  print(part2Result); // Part 2 Answer
}
