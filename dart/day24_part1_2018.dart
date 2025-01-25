
import 'dart:io';
import 'dart:convert';

class Group {
  int units;
  int hitPoints;
  Set<String> weaknesses;
  Set<String> immunities;
  int attackDamage;
  String attackType;
  int initiative;
  String armyType;
  Group? target;
  Group(
      this.units,
      this.hitPoints,
      this.weaknesses,
      this.immunities,
      this.attackDamage,
      this.attackType,
      this.initiative,
      this.armyType);

  int get effectivePower => units * attackDamage;

  int calculateDamage(Group defender) {
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
    int unitsLost = damage ~/ hitPoints;
    units -= unitsLost;
    if (units < 0) {
      units = 0;
    }
  }
}

List<Group> parseGroups(String armyType, List<String> groupDescriptions) {
  List<Group> groups = [];
  for (String desc in groupDescriptions) {
    RegExp groupRegex = RegExp(
        r'(\d+) units each with (\d+) hit points(?: \((.*?)\))? with an attack that does (\d+) (\w+) damage at initiative (\d+)');
    Match? match = groupRegex.firstMatch(desc);
    if (match != null) {
      int units = int.parse(match.group(1)!);
      int hitPoints = int.parse(match.group(2)!);
      String? weaknessesImmunitiesStr = match.group(3);
      int attackDamage = int.parse(match.group(4)!);
      String attackType = match.group(5)!;
      int initiative = int.parse(match.group(6)!);

      Set<String> weaknesses = {};
      Set<String> immunities = {};

      if (weaknessesImmunitiesStr != null) {
        List<String> parts = weaknessesImmunitiesStr.split('; ');
        for (String part in parts) {
          if (part.startsWith('weak to ')) {
            weaknesses = part
                .substring('weak to '.length)
                .split(', ')
                .toSet();
          } else if (part.startsWith('immune to ')) {
            immunities = part
                .substring('immune to '.length)
                .split(', ')
                .toSet();
          }
        }
      }
      groups.add(Group(units, hitPoints, weaknesses, immunities, attackDamage,
          attackType, initiative, armyType));
    }
  }
  return groups;
}

void main() {
  File inputFile = File('input.txt');
  String contents = inputFile.readAsStringSync();
  List<String> lines = LineSplitter.split(contents).toList();

  List<String> immuneSystemDesc = [];
  List<String> infectionDesc = [];

  String currentArmy = '';
  for (String line in lines) {
    if (line == 'Immune System:') {
      currentArmy = 'Immune System';
    } else if (line == 'Infection:') {
      currentArmy = 'Infection';
    } else if (line.isNotEmpty) {
      if (currentArmy == 'Immune System') {
        immuneSystemDesc.add(line);
      } else if (currentArmy == 'Infection') {
        infectionDesc.add(line);
      }
    }
  }

  List<Group> immuneSystemGroups =
      parseGroups('Immune System', immuneSystemDesc);
  List<Group> infectionGroups = parseGroups('Infection', infectionDesc);

  while (immuneSystemGroups.isNotEmpty && infectionGroups.isNotEmpty) {
    // Target Selection
    List<Group> allGroups = [...immuneSystemGroups, ...infectionGroups];
    allGroups.sort((a, b) {
      int powerDiff = b.effectivePower - a.effectivePower;
      if (powerDiff != 0) return powerDiff;
      return b.initiative - a.initiative;
    });

    List<Group> availableInfectionTargets = [...infectionGroups];
    List<Group> availableImmuneTargets = [...immuneSystemGroups];
    Map<Group, Group?> targets = {};

    for (Group attacker in allGroups) {
      if (attacker.units <= 0) continue;
      List<Group> potentialTargets = attacker.armyType == 'Immune System'
          ? availableInfectionTargets
          : availableImmuneTargets;

      Group? bestTarget;
      int maxDamage = -1;
      int maxEffectivePower = -1;
      int maxInitiative = -1;

      for (Group defender in potentialTargets) {
        int damage = attacker.calculateDamage(defender);
        if (damage > maxDamage) {
          maxDamage = damage;
          bestTarget = defender;
          maxEffectivePower = defender.effectivePower;
          maxInitiative = defender.initiative;
        } else if (damage == maxDamage) {
          if (defender.effectivePower > maxEffectivePower) {
            bestTarget = defender;
            maxEffectivePower = defender.effectivePower;
            maxInitiative = defender.initiative;
          } else if (defender.effectivePower == maxEffectivePower) {
            if (defender.initiative > maxInitiative) {
              bestTarget = defender;
              maxInitiative = defender.initiative;
            }
          }
        }
      }
      if (bestTarget != null && maxDamage > 0) {
        targets[attacker] = bestTarget;
        potentialTargets.remove(bestTarget);
      } else {
        targets[attacker] = null;
      }
    }

    // Attacking phase
    List<Group> attackingGroups = [...allGroups];
    attackingGroups.sort((a, b) => b.initiative - a.initiative);
    bool unitsLostInRound = false;

    for (Group attacker in attackingGroups) {
      if (attacker.units <= 0) continue;
      Group? defender = targets[attacker];
      if (defender != null && defender.units > 0) {
        int damage = attacker.calculateDamage(defender);
        int initialUnits = defender.units;
        defender.receiveDamage(damage);
        if (defender.units < initialUnits) {
          unitsLostInRound = true;
        }
      }
    }
    if (!unitsLostInRound) {
      print(0); // Stalemate
      return;
    }


    immuneSystemGroups.removeWhere((group) => group.units <= 0);
    infectionGroups.removeWhere((group) => group.units <= 0);
  }

  int winningUnits = 0;
  if (immuneSystemGroups.isNotEmpty) {
    winningUnits = immuneSystemGroups.fold(0, (sum, group) => sum + group.units);
  } else if (infectionGroups.isNotEmpty) {
    winningUnits = infectionGroups.fold(0, (sum, group) => sum + group.units);
  }

  print(winningUnits);
}
