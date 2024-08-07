import * as fs from 'fs';

interface Group {
  units: number;
  hitPoints: number;
  attackDamage: number;
  attackType: string;
  initiative: number;
  immunities: string[];
  weaknesses: string[];
  attacker?: Group;
  target?: Group;
}

function effectivePower(g: Group): number {
  return g.units * g.attackDamage;
}

function damageDealt(g: Group, e: Group): number {
  if (e.immunities.includes(g.attackType)) {
    return 0;
  }
  if (e.weaknesses.includes(g.attackType)) {
    return effectivePower(g) * 2;
  }
  return effectivePower(g);
}

function parseInput(input: string[]): [Map<number, Group[]>, Group[]] {
  const battle: Map<number, Group[]> = new Map();
  const initiative: Group[] = [];
  let currentArmy = 0;

  const armyName = /^(.*):$/;
  const groupImmunities = /immune to (.*?)[;)]/;
  const groupWeaknesses = /weak to (.*?)[;)]/;
  const groupDescription = /^(\d+) units each with (\d+) hit points.*with an attack that does (\d+) (\w+) damage at initiative (\d+)$/;

  for (const line of input) {
    if (armyName.test(line)) {
      const match = line.match(armyName);
      if (match) {
        currentArmy = match[1] === 'Immune System' ? 1 : 2;
      }
    } else {
      const description = line.match(groupDescription);
      if (description) {
        const group: Group = {
          units: parseInt(description[1]),
          hitPoints: parseInt(description[2]),
          attackDamage: parseInt(description[3]),
          attackType: description[4],
          initiative: parseInt(description[5]),
          immunities: [],
          weaknesses: []
        };

        const immunities = line.match(groupImmunities);
        if (immunities) {
          group.immunities = immunities[1].split(', ');
        }

        const weaknesses = line.match(groupWeaknesses);
        if (weaknesses) {
          group.weaknesses = weaknesses[1].split(', ');
        }

        if (!battle.has(currentArmy)) {
          battle.set(currentArmy, []);
        }
        battle.get(currentArmy)!.push(group);
        initiative.push(group);
      }
    }
  }

  return [battle, initiative];
}

function findTargets(battle: Map<number, Group[]>): void {
  for (const [army, groups] of battle) {
    groups.sort((a, b) => effectivePower(b) - effectivePower(a) || b.initiative - a.initiative);
    for (const group of groups) {
      if (group.units <= 0) continue;
      let mostDamage = 0;
      let targetGroup: Group | undefined;

      for (const [enemyArmy, enemyGroups] of battle) {
        if (army === enemyArmy) continue;
        for (const enemyGroup of enemyGroups) {
          if (enemyGroup.units <= 0 || enemyGroup.attacker || damageDealt(group, enemyGroup) === 0 || damageDealt(group, enemyGroup) < mostDamage) {
            continue;
          }
          if (damageDealt(group, enemyGroup) === mostDamage && targetGroup && (effectivePower(enemyGroup) < effectivePower(targetGroup) || (effectivePower(enemyGroup) === effectivePower(targetGroup) && enemyGroup.initiative < targetGroup.initiative))) {
            continue;
          }
          mostDamage = damageDealt(group, enemyGroup);
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

function attack(initiative: Group[]): void {
  initiative.sort((a, b) => b.initiative - a.initiative);
  for (const group of initiative) {
    if (group.units > 0 && group.target && group.target.units > 0) {
      group.target.units -= Math.floor(damageDealt(group, group.target) / group.target.hitPoints);
    }
    if (group.target) {
      group.target.attacker = undefined;
      group.target = undefined;
    }
  }
}

function clean(battle: Map<number, Group[]>, initiative: Group[]): void {
  for (const army of battle.values()) {
    army.forEach((g, i) => {
      if (g.units <= 0) {
        army.splice(i, 1);
      }
    });
  }
  initiative.forEach((g, i) => {
    if (g.units <= 0) {
      initiative.splice(i, 1);
    }
  });
}

function active(battle: Map<number, Group[]>): boolean {
  for (const army of battle.values()) {
    if (army.every(g => g.units <= 0)) {
      return false;
    }
  }
  return true;
}

function result(battle: Map<number, Group[]>): [number, number] {
  let winner = 0;
  let units = 0;

  for (const [army, groups] of battle) {
    if (groups.some(g => g.units > 0)) {
      winner = army;
      units = groups.reduce((sum, g) => sum + g.units, 0);
    }
  }

  return [winner, units];
}

function totalUnits(battle: Map<number, Group[]>): number {
  return Array.from(battle.values()).flat().reduce((sum, g) => sum + (g.units > 0 ? g.units : 0), 0);
}

function immuneSystemBoost(input: string[]): number {
  let boost = 0;

  while (true) {
    let stalemate = false;
    const [battle, initiative] = parseInput(input);

    battle.get(1)!.forEach(g => g.attackDamage += boost);

    while (active(battle)) {
      const before = totalUnits(battle);

      findTargets(battle);
      attack(initiative);

      if (totalUnits(battle) === before) {
        stalemate = true;
        break;
      }
      clean(battle, initiative);
    }

    if (!stalemate) {
      const [winner, units] = result(battle);
      if (winner === 1) {
        return units;
      }
    }

    boost++;
  }
}

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
console.log(immuneSystemBoost(input));