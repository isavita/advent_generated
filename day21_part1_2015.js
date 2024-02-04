const fs = require('fs');

class Item {
    constructor(cost, damage = 0, armor = 0) {
        this.cost = cost;
        this.damage = damage;
        this.armor = armor;
    }
}

class Character {
    constructor(hitPoints, damage, armor) {
        this.hitPoints = hitPoints;
        this.damage = damage;
        this.armor = armor;
    }
}

const data = fs.readFileSync('input.txt', 'utf8');
const lines = data.split('\n');
const boss = new Character(parseStat(lines[0]), parseStat(lines[1]), parseStat(lines[2]));

const weapons = [
    new Item(8, 4),
    new Item(10, 5),
    new Item(25, 6),
    new Item(40, 7),
    new Item(74, 8)
];

const armors = [
    new Item(0, 0),
    new Item(13, 0, 1),
    new Item(31, 0, 2),
    new Item(53, 0, 3),
    new Item(75, 0, 4),
    new Item(102, 0, 5)
];

const rings = [
    new Item(0),
    new Item(25, 1),
    new Item(50, 2),
    new Item(100, 3),
    new Item(20, 0, 1),
    new Item(40, 0, 2),
    new Item(80, 0, 3)
];

let minCost = Number.MAX_SAFE_INTEGER;

weapons.forEach(w => {
    armors.forEach(a => {
        for (let ri = 0; ri < rings.length; ri++) {
            for (let rj = ri + 1; rj < rings.length; rj++) {
                const player = new Character(100, w.damage, a.armor);
                player.damage += rings[ri].damage + rings[rj].damage;
                player.armor += rings[ri].armor + rings[rj].armor;
                const cost = w.cost + a.cost + rings[ri].cost + rings[rj].cost;
                if (playerWins(player, boss) && cost < minCost) {
                    minCost = cost;
                }
            }
        }
    });
});

console.log(minCost);

function parseStat(line) {
    const parts = line.split(': ');
    const value = parseInt(parts[1]);
    if (isNaN(value)) {
        console.error(`Invalid stat in input: ${parts[1]}`);
        process.exit(1);
    }
    return value;
}

function playerWins(player, boss) {
    const playerDamage = Math.max(1, player.damage - boss.armor);
    const bossDamage = Math.max(1, boss.damage - player.armor);

    const playerTurns = Math.ceil(boss.hitPoints / playerDamage);
    const bossTurns = Math.ceil(player.hitPoints / bossDamage);

    return playerTurns <= bossTurns;
}