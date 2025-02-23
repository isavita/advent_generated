
import * as fs from 'fs';

class Group {
    units: number;
    hitPoints: number;
    attackDamage: number;
    attackType: string;
    initiative: number;
    immunities: string[];
    weaknesses: string[];
    attacker: Group | null;
    target: Group | null;

    constructor(units: number, hitPoints: number, attackDamage: number, attackType: string, initiative: number, immunities: string[], weaknesses: string[]) {
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

    effectivePower(): number {
        return this.units * this.attackDamage;
    }

    damageDealt(enemy: Group): number {
        if (enemy.immunities.includes(this.attackType)) {
            return 0;
        }
        if (enemy.weaknesses.includes(this.attackType)) {
            return this.effectivePower() * 2;
        }
        return this.effectivePower();
    }

    toString(): string {
        let out = `${this.units} units each with ${this.hitPoints} hit points`;
        if (this.immunities.length > 0 || this.weaknesses.length > 0) {
            out += " (";
            if (this.immunities.length > 0) {
                out += "immune to " + this.immunities.join(" and ");
                if (this.weaknesses.length > 0) {
                    out += "; ";
                }
            }
            if (this.weaknesses.length > 0) {
                out += "weak to " + this.weaknesses.join(" and ");
            }
            out += ")";
        }
        out += ` with an attack that does ${this.attackDamage} ${this.attackType} damage at initiative ${this.initiative}`;
        return out;
    }
}

class Initiative {
    groups: Group[];

    constructor(groups: Group[]) {
        this.groups = groups;
    }

    attack(): void {
        this.groups.sort((a, b) => b.initiative - a.initiative);
        for (const group of this.groups) {
            if (group.units > 0 && group.target && group.target.units > 0) {
                group.target.units -= Math.floor(group.damageDealt(group.target) / group.target.hitPoints);
                if (group.target.units < 0) {
                    group.target.units = 0;
                }
            }
            if (group.target) {
                group.target.attacker = null;
                group.target = null;
            }
        }
    }

    clean(): void {
        this.groups = this.groups.filter(g => g.units > 0);
        this.groups.sort((a, b) => b.initiative - a.initiative);
    }
}

class Army {
    groups: Group[];

    constructor(groups: Group[]) {
        this.groups = groups;
    }

    alive(): boolean {
        return this.groups.some(g => g.units > 0);
    }

    boost(amount: number): void {
        for (const g of this.groups) {
            g.attackDamage += amount;
        }
    }
}

class Battlefield {
    armies: { [armyId: number]: Army };

    constructor(armies: { [armyId: number]: Army }) {
        this.armies = armies;
    }

    findTargets(): void {
        for (const armyId in this.armies) {
            const army = this.armies[armyId];
            army.groups.sort((a, b) => {
                const powerA = a.effectivePower();
                const powerB = b.effectivePower();
                if (powerA !== powerB) {
                    return powerB - powerA;
                }
                return b.initiative - a.initiative;
            });

            for (const group of army.groups) {
                let mostDamage = 0;
                let targetGroup: Group | null = null;

                for (const enemyArmyId in this.armies) {
                    if (armyId === enemyArmyId) {
                        continue;
                    }

                    const enemyArmy = this.armies[enemyArmyId];
                    for (const enemyGroup of enemyArmy.groups) {
                        if (enemyGroup.units <= 0 || enemyGroup.attacker || group.damageDealt(enemyGroup) === 0) {
                            continue;
                        }

                        const damage = group.damageDealt(enemyGroup);

                        if (damage > mostDamage) {
                            mostDamage = damage;
                            targetGroup = enemyGroup;
                        } else if (damage === mostDamage && targetGroup) {
                            if (enemyGroup.effectivePower() > targetGroup.effectivePower()) {
                                targetGroup = enemyGroup;
                            } else if (enemyGroup.effectivePower() === targetGroup.effectivePower() && enemyGroup.initiative > targetGroup.initiative) {
                                targetGroup = enemyGroup;
                            }
                        }
                    }
                }

                if (targetGroup) {
                    group.target = targetGroup;
                    targetGroup.attacker = group;
                }
            }
        }
    }

    clean(): void {
        for (const armyId in this.armies) {
            this.armies[armyId].groups = this.armies[armyId].groups.filter(g => g.units > 0);
        }
    }

    active(): boolean {
        return Object.values(this.armies).every(army => army.alive());
    }

    result(): [number, number] {
        let winner = 0;
        let units = 0;
        for (const armyId in this.armies) {
            const army = this.armies[armyId];
            if (army.alive()) {
                winner = parseInt(armyId);
                units = army.groups.reduce((sum, g) => sum + g.units, 0);
            }
        }
        return [winner, units];
    }

    totalUnits(): number {
        let total = 0;
        for (const armyId in this.armies) {
            const army = this.armies[armyId];
            total += army.groups.reduce((sum, g) => sum + g.units, 0);
        }
        return total;
    }
}

function parseInput(inputData: string): [Battlefield, Initiative] {
    const armies: { [armyId: number]: Army } = {};
    const initiative: Group[] = [];
    let currentArmyId = 0;

    const armyNamePattern = /^(.*):$/;
    const groupImmunitiesPattern = /immune to (.*?)[;)]/;
    const groupWeaknessesPattern = /weak to (.*?)[;)]/;
    const groupDescriptionPattern = /^(\d+) units each with (\d+) hit points.*with an attack that does (\d+) (\w+) damage at initiative (\d+)$/;

    const lines = inputData.split('\n');

    for (const line of lines) {
        const armyNameMatch = line.match(armyNamePattern);
        if (armyNameMatch) {
            const armyName = armyNameMatch[1];
            if (armyName === "Immune System") {
                currentArmyId = 1;
            } else if (armyName === "Infection") {
                currentArmyId = 2;
            } else {
                throw new Error(`Unknown army: ${armyName}`);
            }
        } else {
            if (currentArmyId <= 0 || currentArmyId >= 3) {
                throw new Error(`Tried to assign group to invalid army: ${currentArmyId}`);
            }
            const groupDescriptionMatch = line.match(groupDescriptionPattern);
            if (!groupDescriptionMatch) {
                continue;
            }
            const units = parseInt(groupDescriptionMatch[1]);
            const hitPoints = parseInt(groupDescriptionMatch[2]);
            const attackDamage = parseInt(groupDescriptionMatch[3]);
            const attackType = groupDescriptionMatch[4];
            const initiativeValue = parseInt(groupDescriptionMatch[5]);

            const immunitiesMatch = line.match(groupImmunitiesPattern);
            const immunities = immunitiesMatch ? immunitiesMatch[1].split(", ") : [];
            const weaknessesMatch = line.match(groupWeaknessesPattern);
            const weaknesses = weaknessesMatch ? weaknessesMatch[1].split(", ") : [];

            const group = new Group(units, hitPoints, attackDamage, attackType, initiativeValue, immunities, weaknesses);
            if (!armies[currentArmyId]) {
                armies[currentArmyId] = new Army([]);
            }
            armies[currentArmyId].groups.push(group);
            initiative.push(group);
        }
    }

    return [new Battlefield(armies), new Initiative(initiative)];
}

function conditionFight(inputData: string): number {
    const [battle, initiative] = parseInput(inputData);
    let previousUnits = -1;
    while (battle.active()) {
        battle.findTargets();
        initiative.attack();
        battle.clean();
        initiative.clean();
        const currentUnits = battle.totalUnits();
        if(currentUnits === previousUnits){
            return 0;
        }
        previousUnits = currentUnits;
    }
    const [, units] = battle.result();
    return units;
}

function main(): void {
    const inputData = fs.readFileSync("input.txt", "utf-8").trim();
    console.log(conditionFight(inputData));
}

main();
