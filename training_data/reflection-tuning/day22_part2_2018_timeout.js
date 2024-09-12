const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const depth = parseInt(input[0].split(': ')[1]);
const [targetX, targetY] = input[1].split(': ')[1].split(',').map(Number);

const MOD = 20183;

function geologicIndex(x, y, erosionLevels) {
    if ((x === 0 && y === 0) || (x === targetX && y === targetY)) return 0;
    if (y === 0) return x * 16807;
    if (x === 0) return y * 48271;
    return erosionLevels.get(`${x-1},${y}`) * erosionLevels.get(`${x},${y-1}`);
}

function erosionLevel(index) {
    return (index + depth) % MOD;
}

function regionType(erosion) {
    return erosion % 3;
}

// Part 1
function calculateRiskLevel() {
    let risk = 0;
    const erosionLevels = new Map();

    for (let y = 0; y <= targetY; y++) {
        for (let x = 0; x <= targetX; x++) {
            const index = geologicIndex(x, y, erosionLevels);
            const erosion = erosionLevel(index);
            erosionLevels.set(`${x},${y}`, erosion);
            risk += regionType(erosion);
        }
    }

    return risk;
}

// Part 2
const ROCKY = 0, WET = 1, NARROW = 2;
const TORCH = 0, CLIMBING = 1, NEITHER = 2;
const TOOLS = [TORCH, CLIMBING, NEITHER];

function isToolValid(type, tool) {
    if (type === ROCKY) return tool !== NEITHER;
    if (type === WET) return tool !== TORCH;
    return tool !== CLIMBING;
}

function shortestPath() {
    const queue = [[0, 0, 0, TORCH]]; // [time, x, y, tool]
    const visited = new Set();
    const erosionLevels = new Map();

    while (queue.length > 0) {
        queue.sort((a, b) => a[0] - b[0]);
        const [time, x, y, tool] = queue.shift();

        if (x === targetX && y === targetY && tool === TORCH) {
            return time;
        }

        const key = `${x},${y},${tool}`;
        if (visited.has(key)) continue;
        visited.add(key);

        const currentType = regionType(erosionLevel(geologicIndex(x, y, erosionLevels)));

        // Try switching tools
        for (const newTool of TOOLS) {
            if (newTool !== tool && isToolValid(currentType, newTool)) {
                queue.push([time + 7, x, y, newTool]);
            }
        }

        // Try moving to adjacent regions
        for (const [dx, dy] of [[0, 1], [1, 0], [0, -1], [-1, 0]]) {
            const newX = x + dx, newY = y + dy;
            if (newX < 0 || newY < 0) continue;

            const newType = regionType(erosionLevel(geologicIndex(newX, newY, erosionLevels)));
            if (isToolValid(newType, tool)) {
                queue.push([time + 1, newX, newY, tool]);
            }
        }
    }
}

console.log("Part 1:", calculateRiskLevel());
console.log("Part 2:", shortestPath());
