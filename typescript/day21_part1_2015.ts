import * as fs from 'fs';

// Define the weapons, armor, and rings available in the shop
const weapons = [
    { name: 'Dagger', cost: 8, damage: 4, armor: 0 },
    { name: 'Shortsword', cost: 10, damage: 5, armor: 0 },
    { name: 'Warhammer', cost: 25, damage: 6, armor: 0 },
    { name: 'Longsword', cost: 40, damage: 7, armor: 0 },
    { name: 'Greataxe', cost: 74, damage: 8, armor: 0 },
];

const armors = [
    { name: 'Leather', cost: 13, damage: 0, armor: 1 },
    { name: 'Chainmail', cost: 31, damage: 0, armor: 2 },
    { name: 'Splintmail', cost: 53, damage: 0, armor: 3 },
    { name: 'Bandedmail', cost: 75, damage: 0, armor: 4 },
    { name: 'Platemail', cost: 102, damage: 0, armor: 5 },
    { name: 'None', cost: 0, damage: 0, armor: 0 }, // No armor option
];

const rings = [
    { name: 'Damage +1', cost: 25, damage: 1, armor: 0 },
    { name: 'Damage +2', cost: 50, damage: 2, armor: 0 },
    { name: 'Damage +3', cost: 100, damage: 3, armor: 0 },
    { name: 'Defense +1', cost: 20, damage: 0, armor: 1 },
    { name: 'Defense +2', cost: 40, damage: 0, armor: 2 },
    { name: 'Defense +3', cost: 80, damage: 0, armor: 3 },
    { name: 'None', cost: 0, damage: 0, armor: 0 }, // No ring option
];

// Function to simulate the fight
function simulateFight(player: { hp: number, damage: number, armor: number }, boss: { hp: number, damage: number, armor: number }): boolean {
    while (player.hp > 0 && boss.hp > 0) {
        // Player attacks
        boss.hp -= Math.max(1, player.damage - boss.armor);
        if (boss.hp <= 0) return true;

        // Boss attacks
        player.hp -= Math.max(1, boss.damage - player.armor);
        if (player.hp <= 0) return false;
    }
    return false;
}

// Function to find the least amount of gold to win
function findLeastGoldToWin(boss: { hp: number, damage: number, armor: number }): number {
    let minGold = Number.MAX_SAFE_INTEGER;

    for (const weapon of weapons) {
        for (const armor of armors) {
            for (let i = 0; i < rings.length; i++) {
                for (let j = i + 1; j < rings.length; j++) {
                    const ring1 = rings[i];
                    const ring2 = rings[j];

                    const totalCost = weapon.cost + armor.cost + ring1.cost + ring2.cost;
                    const totalDamage = weapon.damage + armor.damage + ring1.damage + ring2.damage;
                    const totalArmor = weapon.armor + armor.armor + ring1.armor + ring2.armor;

                    const player = { hp: 100, damage: totalDamage, armor: totalArmor };
                    if (simulateFight(player, { ...boss })) {
                        minGold = Math.min(minGold, totalCost);
                    }
                }
            }
        }
    }

    return minGold;
}

// Read input from file
const input = fs.readFileSync('input.txt', 'utf-8');
const [hp, damage, armor] = input.split('\n').map(line => parseInt(line.split(': ')[1], 10));

// Boss stats
const boss = { hp, damage, armor };

// Find and print the least amount of gold to win
const leastGold = findLeastGoldToWin(boss);
console.log(leastGold);