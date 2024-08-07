import * as fs from 'fs';

type Item = { cost: number, damage: number, armor: number };
type Equipment = { weapon: Item, armor?: Item, rings: Item[] };
type Stats = { hitPoints: number, damage: number, armor: number };

const weapons: Item[] = [
    { cost: 8, damage: 4, armor: 0 },
    { cost: 10, damage: 5, armor: 0 },
    { cost: 25, damage: 6, armor: 0 },
    { cost: 40, damage: 7, armor: 0 },
    { cost: 74, damage: 8, armor: 0 },
];

const armors: Item[] = [
    { cost: 13, damage: 0, armor: 1 },
    { cost: 31, damage: 0, armor: 2 },
    { cost: 53, damage: 0, armor: 3 },
    { cost: 75, damage: 0, armor: 4 },
    { cost: 102, damage: 0, armor: 5 },
];

const rings: Item[] = [
    { cost: 25, damage: 1, armor: 0 },
    { cost: 50, damage: 2, armor: 0 },
    { cost: 100, damage: 3, armor: 0 },
    { cost: 20, damage: 0, armor: 1 },
    { cost: 40, damage: 0, armor: 2 },
    { cost: 80, damage: 0, armor: 3 },
];

function simulateBattle(player: Stats, boss: Stats): boolean {
    let playerHP = player.hitPoints;
    let bossHP = boss.hitPoints;

    while (true) {
        bossHP -= Math.max(player.damage - boss.armor, 1);
        if (bossHP <= 0) return true;

        playerHP -= Math.max(boss.damage - player.armor, 1);
        if (playerHP <= 0) return false;
    }
}

function calculateCost(equipment: Equipment): number {
    return equipment.weapon.cost + (equipment.armor ? equipment.armor.cost : 0) +
        equipment.rings.reduce((sum, ring) => sum + ring.cost, 0);
}

function generateEquipment(): Equipment[] {
    const equipments: Equipment[] = [];

    for (const weapon of weapons) {
        for (const armor of [null, ...armors]) {
            for (const ring1 of rings) {
                for (const ring2 of [null, ...rings].filter(r => r === null || r.cost < ring1.cost)) {
                    equipments.push({
                        weapon,
                        armor: armor ? armor : undefined,
                        rings: [ring1, ring2].filter(r => r !== null) as Item[],
                    });
                }
            }
        }
    }

    return equipments;
}

function main(): void {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const [hitPoints, damage, armor] = input.split('\n').map(line => parseInt(line.split(': ')[1]));

    const boss: Stats = { hitPoints, damage, armor };
    const playerBase: Stats = { hitPoints: 100, damage: 0, armor: 0 };

    let minCost = Infinity;
    let maxCost = 0;

    const equipments = generateEquipment();

    for (const equipment of equipments) {
        const player: Stats = {
            hitPoints: playerBase.hitPoints,
            damage: equipment.weapon.damage + (equipment.armor ? equipment.armor.damage : 0) +
                equipment.rings.reduce((sum, ring) => sum + ring.damage, 0),
            armor: equipment.weapon.armor + (equipment.armor ? equipment.armor.armor : 0) +
                equipment.rings.reduce((sum, ring) => sum + ring.armor, 0),
        };

        const cost = calculateCost(equipment);
        if (simulateBattle(player, boss)) {
            minCost = Math.min(minCost, cost);
        } else {
            maxCost = Math.max(maxCost, cost);
        }
    }

    console.log(`Least amount of gold to win: ${minCost}`);
    console.log(`Most amount of gold to lose: ${maxCost}`);
}

main();