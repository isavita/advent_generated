const fs = require('fs');

// Read input from input.txt
const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const depth = parseInt(input[0].split(': ')[1], 10);
const target = input[1].split(': ')[1].split(',').map(Number);

// Calculate geologic index
function getGeologicIndex(x, y, target) {
    if (x === 0 && y === 0) return 0;
    if (x === target[0] && y === target[1]) return 0;
    if (y === 0) return x * 16807;
    if (x === 0) return y * 48271;
    return erosionLevels[y][x - 1] * erosionLevels[y - 1][x];
}

// Calculate erosion level
function getErosionLevel(geologicIndex, depth) {
    return (geologicIndex + depth) % 20183;
}

// Calculate region type
function getRegionType(erosionLevel) {
    return erosionLevel % 3;
}

// Initialize erosion levels and region types
const erosionLevels = [];
const regionTypes = [];
const maxX = target[0] + 100;
const maxY = target[1] + 100;

for (let y = 0; y <= maxY; y++) {
    erosionLevels[y] = [];
    regionTypes[y] = [];
    for (let x = 0; x <= maxX; x++) {
        const geologicIndex = getGeologicIndex(x, y, target);
        const erosionLevel = getErosionLevel(geologicIndex, depth);
        erosionLevels[y][x] = erosionLevel;
        regionTypes[y][x] = getRegionType(erosionLevel);
    }
}

// Calculate risk level
let riskLevel = 0;
for (let y = 0; y <= target[1]; y++) {
    for (let x = 0; x <= target[0]; x++) {
        riskLevel += regionTypes[y][x];
    }
}

console.log('Total risk level:', riskLevel);

// Part Two: Find the shortest path
function findShortestPath(target) {
    const queue = [{ x: 0, y: 0, tool: 'torch', minutes: 0 }];
    const visited = new Set();
    const targetKey = `${target[0]},${target[1]},torch`;

    while (queue.length > 0) {
        queue.sort((a, b) => a.minutes - b.minutes);
        const { x, y, tool, minutes } = queue.shift();
        const key = `${x},${y},${tool}`;

        if (visited.has(key)) continue;
        visited.add(key);

        if (x === target[0] && y === target[1] && tool === 'torch') {
            return minutes;
        }

        const regionType = regionTypes[y][x];
        const tools = {
            rocky: ['climbing', 'torch'],
            wet: ['climbing', 'neither'],
            narrow: ['torch', 'neither']
        }[['rocky', 'wet', 'narrow'][regionType]];

        // Try switching tools
        for (const newTool of tools) {
            if (newTool !== tool) {
                queue.push({ x, y, tool: newTool, minutes: minutes + 7 });
            }
        }

        // Try moving to adjacent regions
        for (const [dx, dy] of [[0, 1], [1, 0], [0, -1], [-1, 0]]) {
            const newX = x + dx;
            const newY = y + dy;
            if (newX >= 0 && newY >= 0 && newX <= maxX && newY <= maxY) {
                const newRegionType = regionTypes[newY][newX];
                const newTools = {
                    rocky: ['climbing', 'torch'],
                    wet: ['climbing', 'neither'],
                    narrow: ['torch', 'neither']
                }[['rocky', 'wet', 'narrow'][newRegionType]];

                if (newTools.includes(tool)) {
                    queue.push({ x: newX, y: newY, tool, minutes: minutes + 1 });
                }
            }
        }
    }

    return -1; // Should not reach here
}

const shortestPath = findShortestPath(target);
console.log('Fewest number of minutes to reach the target:', shortestPath);