const fs = require('fs');

function main() {
    const data = fs.readFileSync('input.txt', 'utf8');
    const [depth, target] = parseInput(data);

    const cave = makeCaveSystem(depth, target);
    const riskLevel = calculateRiskLevel(cave, target);
    console.log("Total Risk Level:", riskLevel);
}

function parseInput(data) {
    const lines = data.split("\n");
    const depth = parseInt(lines[0].split(" ")[1], 10);
    const coords = lines[1].split(" ")[1];
    const parts = coords.split(",");
    const x = parseInt(parts[0], 10);
    const y = parseInt(parts[1], 10);
    return [depth, [x, y]];
}

function makeCaveSystem(depth, target) {
    const cave = new Array(target[1] + 1).fill().map(() => new Array(target[0] + 1));
    for (let y = 0; y < cave.length; y++) {
        for (let x = 0; x < cave[y].length; x++) {
            let geologicIndex = 0;
            if ((x === 0 && y === 0) || (x === target[0] && y === target[1])) {
                geologicIndex = 0;
            } else if (y === 0) {
                geologicIndex = x * 16807;
            } else if (x === 0) {
                geologicIndex = y * 48271;
            } else {
                geologicIndex = cave[y][x - 1] * cave[y - 1][x];
            }
            cave[y][x] = (geologicIndex + depth) % 20183;
        }
    }
    return cave;
}

function calculateRiskLevel(cave, target) {
    let riskLevel = 0;
    for (let y = 0; y <= target[1]; y++) {
        for (let x = 0; x <= target[0]; x++) {
            riskLevel += cave[y][x] % 3;
        }
    }
    return riskLevel;
}

main();