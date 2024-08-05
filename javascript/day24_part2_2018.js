const fs = require('fs');

class Group {
    constructor(units, hitPoints, attackDamage, attackType, initiative, immunities = [], weaknesses = []) {
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

    toString() {
        let out = `${this.units} units each with ${this.hitPoints} hit points`;
        if (this.immunities.length || this.weaknesses.length) {
            out += " (";
            if (this.immunities.length) {
                out += `immune to ${this.immunities.join(" and ")}`;
                if (this.weaknesses.length) {
                    out += "; ";
                }
            }
            if (this.weaknesses.length) {
                out += `weak to ${this.weaknesses.join(" and ")}`;
            }
            out += ")";
        }
        out += ` with an attack that does ${this.attackDamage} ${this.attackType} damage at initiative ${this.initiative}`;
        return out;
    }
}

class Initiative {
    constructor(groups) {
        this.groups = groups;
    }

    attack() {
        this.groups.sort((a, b) => b.initiative - a.initiative);

        for (const group of this.groups) {
            if (group.units > 0 && group.target && group.target.units > 0) {
                group.target.units -= Math.floor(group.damageDealt(group.target) / group.target.hitPoints);
            }
            if (group.target) {
                group.target.attacker = null;
                group.target = null;
            }
        }
    }

    clean() {
        this.groups = this.groups.filter(g => g.units > 0);
        this.groups.sort((a, b) => b.initiative - a.initiative);
    }
}

class Army {
    constructor(groups) {
        this.groups = groups;
    }

    alive() {
        return this.groups.some(g => g.units > 0);
    }

    boost(amount) {
        for (const group of this.groups) {
            group.attackDamage += amount;
        }
    }

    sort() {
        this.groups.sort((a, b) => {
            if (a.effectivePower() > b.effectivePower()) {
                return -1;
            }
            if (a.effectivePower() < b.effectivePower()) {
                return 1;
            }
            return b.initiative - a.initiative;
        });
    }
}

const ArmyImmuneSystem = 1;
const ArmyInfection = 2;

function prepareForBattle(input) {
    const battle = { [ArmyImmuneSystem]: [], [ArmyInfection]: [] };
    let currentArmy = 0;
    const initiative = [];

    for (const line of input) {
        const armyMatch = line.match(/^(.*):$/);
        if (armyMatch) {
            currentArmy = armyMatch[1] === "Immune System" ? ArmyImmuneSystem : ArmyInfection;
        } else {
            const descriptionMatch = line.match(/^(\d+) units each with (\d+) hit points.*with an attack that does (\d+) (\w+) damage at initiative (\d+)$/);
            if (descriptionMatch) {
                const group = new Group(
                    parseInt(descriptionMatch[1], 10),
                    parseInt(descriptionMatch[2], 10),
                    parseInt(descriptionMatch[3], 10),
                    descriptionMatch[4],
                    parseInt(descriptionMatch[5], 10)
                );

                const immunitiesMatch = line.match(/immune to (.*?)[;)]/);
                if (immunitiesMatch) {
                    group.immunities = immunitiesMatch[1].split(", ");
                }

                const weaknessesMatch = line.match(/weak to (.*?)[;)]/);
                if (weaknessesMatch) {
                    group.weaknesses = weaknessesMatch[1].split(", ");
                }

                battle[currentArmy].push(group);
                initiative.push(group);
            }
        }
    }

    return {
        battle: {
            [ArmyImmuneSystem]: new Army(battle[ArmyImmuneSystem]),
            [ArmyInfection]: new Army(battle[ArmyInfection])
        },
        initiative: new Initiative(initiative)
    };
}

function findTargets(battle) {
    for (const army of Object.values(battle)) {
        army.sort();
        for (const group of army.groups) {
            let mostDamage = 0;
            let targetGroup = null;

            for (const enemyArmy of Object.values(battle)) {
                if (enemyArmy === army || group.units <= 0) {
                    continue;
                }

                for (const enemyGroup of enemyArmy.groups) {
                    if (enemyGroup.units <= 0 || enemyGroup.attacker || group.damageDealt(enemyGroup) === 0 || group.damageDealt(enemyGroup) < mostDamage) {
                        continue;
                    }
                    if (group.damageDealt(enemyGroup) === mostDamage && targetGroup && (enemyGroup.effectivePower() < targetGroup.effectivePower() || (enemyGroup.effectivePower() === targetGroup.effectivePower() && enemyGroup.initiative < targetGroup.initiative))) {
                        continue;
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

function cleanBattle(battle) {
    for (const army of Object.values(battle)) {
        army.groups = army.groups.filter(g => g.units > 0);
    }
}

function activeBattle(battle) {
    return Object.values(battle).every(army => army.alive());
}

function resultBattle(battle) {
    let winner = null;
    let units = 0;

    for (const [armyId, army] of Object.entries(battle)) {
        if (army.alive()) {
            winner = parseInt(armyId, 10);
            units = army.groups.reduce((sum, g) => sum + g.units, 0);
        }
    }

    return [winner, units];
}

function totalUnits(battle) {
    return Object.values(battle).flatMap(army => army.groups).reduce((sum, group) => sum + (group.units > 0 ? group.units : 0), 0);
}

function immuneSystemBoost(input) {
    let boost = 0;

    while (true) {
        let stalemate = false;
        const { battle, initiative } = prepareForBattle(input);

        battle[ArmyImmuneSystem].boost(boost);

        while (activeBattle(battle)) {
            const before = totalUnits(battle);

            findTargets(battle);
            initiative.attack();

            if (totalUnits(battle) === before) {
                stalemate = true;
                break;
            }
            cleanBattle(battle);
            initiative.clean();
        }

        if (!stalemate) {
            const [winner, units] = resultBattle(battle);
            if (winner === ArmyImmuneSystem) {
                return units;
            }
        }

        boost++;
    }
}

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
console.log(immuneSystemBoost(input));