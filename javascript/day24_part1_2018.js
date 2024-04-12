const fs = require('fs');

class Group {
  constructor(units, hitPoints, attackDamage, attackType, initiative, immunities, weaknesses) {
    this.units = units;
    this.hitPoints = hitPoints;
    this.attackDamage = attackDamage;
    this.attackType = attackType;
    this.initiative = initiative;
    this.immunities = immunities;
    this.weaknesses = weaknesses;
    this.attacker = null;
    this.target = null;
  }

  effectivePower() {
    return this.units * this.attackDamage;
  }

  damageDealt(enemy) {
    if (enemy.immunities.includes(this.attackType)) {
      return 0;
    }
    if (enemy.weaknesses.includes(this.attackType)) {
      return this.effectivePower() * 2;
    }
    return this.effectivePower();
  }
}

const ArmyImmuneSystem = 1;
const ArmyInfection = 2;
const ArmyCount = 2;

const StringArmies = {
  'Immune System': ArmyImmuneSystem,
  'Infection': ArmyInfection,
};

const armyName = /^(.*):$/;
const groupImmunities = /immune to (.*?)[;)]/;
const groupWeaknesses = /weak to (.*?)[;)]/;
const groupDescription = /^(\d+) units each with (\d+) hit points.*with an attack that does (\d+) (\w+) damage at initiative (\d+)$/;

const descriptionCount = 1;
const descriptionHitPoints = 2;
const descriptionDamage = 3;
const descriptionDamageType = 4;
const descriptionInitiative = 5;

function prepareForBattle(input) {
  const initiative = [];
  const battle = {};
  let currentArmy = 0;

  for (const line of input) {
    const armyMatch = line.match(armyName);
    if (armyMatch) {
      const id = StringArmies[armyMatch[1]];
      if (id) {
        currentArmy = id;
      } else {
        throw new Error(`Unknown army: ${armyMatch[1]}`);
      }
    } else {
      if (currentArmy <= 0 || currentArmy > ArmyCount) {
        throw new Error(`Tried to assign group to invalid army: ${currentArmy}`);
      }
      const description = line.match(groupDescription);
      if (!description) {
        continue;
      }

      const group = new Group(
        parseInt(description[descriptionCount]),
        parseInt(description[descriptionHitPoints]),
        parseInt(description[descriptionDamage]),
        description[descriptionDamageType],
        parseInt(description[descriptionInitiative]),
        [],
        []
      );

      const immunities = line.match(groupImmunities);
      if (immunities) {
        group.immunities = immunities[1].split(', ');
      }

      const weaknesses = line.match(groupWeaknesses);
      if (weaknesses) {
        group.weaknesses = weaknesses[1].split(', ');
      }

      if (!battle[currentArmy]) {
        battle[currentArmy] = [];
      }
      battle[currentArmy].push(group);
      initiative.push(group);
    }
  }

  return [battle, initiative];
}

function findTargets(battle) {
  for (const army in battle) {
    battle[army].sort((a, b) => {
      if (a.effectivePower() !== b.effectivePower()) {
        return b.effectivePower() - a.effectivePower();
      }
      return b.initiative - a.initiative;
    });

    for (const group of battle[army]) {
      if (group.units <= 0) {
        continue;
      }

      let mostDamage = 0;
      let targetGroup = null;

      for (const enemyArmy in battle) {
        if (army === enemyArmy) {
          continue;
        }

        for (const enemyGroup of battle[enemyArmy]) {
          if (enemyGroup.units <= 0 || enemyGroup.attacker || group.damageDealt(enemyGroup) === 0 || group.damageDealt(enemyGroup) < mostDamage) {
            continue;
          }
          if (group.damageDealt(enemyGroup) === mostDamage && targetGroup) {
            if (enemyGroup.effectivePower() < targetGroup.effectivePower()) {
              continue;
            }
            if (enemyGroup.effectivePower() === targetGroup.effectivePower() && enemyGroup.initiative < targetGroup.initiative) {
              continue;
            }
          }
          mostDamage = group.damageDealt(enemyGroup);
          targetGroup = enemyGroup;
        }
      }

      if (targetGroup) {
        group.target = targetGroup;
        targetGroup.attacker = group;
      }
    }
  }
}

function attack(initiative) {
  initiative.sort((a, b) => b.initiative - a.initiative);

  for (const group of initiative) {
    if (group.units > 0 && group.target && group.target.units > 0) {
      group.target.units -= Math.floor(group.damageDealt(group.target) / group.target.hitPoints);
    }
    if (group.target) {
      group.target.attacker = null;
      group.target = null;
    }
  }
}

function cleanBattlefield(battle, initiative) {
  for (const army in battle) {
    battle[army] = battle[army].filter(group => group.units > 0);
  }
  initiative = initiative.filter(group => group.units > 0);
  initiative.sort((a, b) => b.initiative - a.initiative);
}

function isBattleActive(battle) {
  for (const army in battle) {
    if (!battle[army].some(group => group.units > 0)) {
      return false;
    }
  }
  return true;
}

function getBattleResult(battle) {
  let winner = 0;
  let units = 0;

  for (const army in battle) {
    if (battle[army].some(group => group.units > 0)) {
      winner = parseInt(army);
      units = battle[army].reduce((sum, group) => sum + group.units, 0);
    }
  }

  return [winner, units];
}

function conditionFight(input) {
  const [battle, initiative] = prepareForBattle(input);

  while (isBattleActive(battle)) {
    findTargets(battle);
    attack(initiative);
    cleanBattlefield(battle, initiative);
  }

  const [_, units] = getBattleResult(battle);
  return units;
}

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
console.log(conditionFight(input));