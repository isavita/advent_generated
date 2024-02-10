
const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim();
const serial = parseInt(data);

const gridSize = 300;
const grid = Array.from({ length: gridSize }, () => Array(gridSize).fill(0));

for (let y = 0; y < gridSize; y++) {
    for (let x = 0; x < gridSize; x++) {
        const rackID = x + 11;
        let powerLevel = rackID * (y + 1);
        powerLevel += serial;
        powerLevel *= rackID;
        powerLevel = Math.floor(powerLevel / 100) % 10;
        powerLevel -= 5;
        grid[y][x] = powerLevel;
    }
}

let maxPower = -1 << 31;
let maxX = 0;
let maxY = 0;

for (let y = 0; y < gridSize - 2; y++) {
    for (let x = 0; x < gridSize - 2; x++) {
        let totalPower = 0;
        for (let dy = 0; dy < 3; dy++) {
            for (let dx = 0; dx < 3; dx++) {
                totalPower += grid[y + dy][x + dx];
            }
        }
        if (totalPower > maxPower) {
            maxPower = totalPower;
            maxX = x + 1;
            maxY = y + 1;
        }
    }
}

console.log(`${maxX},${maxY}`);
