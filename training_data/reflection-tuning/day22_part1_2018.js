const fs = require('fs');

// Read input from file
const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const depth = parseInt(input[0].split(': ')[1]);
const [targetX, targetY] = input[1].split(': ')[1].split(',').map(Number);

// Constants
const MOD = 20183;
const X_MULT = 16807;
const Y_MULT = 48271;

// Function to calculate geologic index
function geologicIndex(x, y, erosionLevels) {
    if ((x === 0 && y === 0) || (x === targetX && y === targetY)) return 0;
    if (y === 0) return x * X_MULT;
    if (x === 0) return y * Y_MULT;
    return erosionLevels[y][x-1] * erosionLevels[y-1][x];
}

// Function to calculate erosion level
function erosionLevel(index) {
    return (index + depth) % MOD;
}

// Function to determine region type
function regionType(level) {
    return level % 3;
}

// Calculate total risk level
let totalRisk = 0;
const erosionLevels = Array(targetY + 1).fill().map(() => Array(targetX + 1));

for (let y = 0; y <= targetY; y++) {
    for (let x = 0; x <= targetX; x++) {
        const index = geologicIndex(x, y, erosionLevels);
        erosionLevels[y][x] = erosionLevel(index);
        totalRisk += regionType(erosionLevels[y][x]);
    }
}

console.log(totalRisk);
