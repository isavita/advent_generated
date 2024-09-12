class PriorityQueue {
    constructor() {
        this.elements = [];
    }

    enqueue(element, priority) {
        this.elements.push({element, priority});
        this.elements.sort((a, b) => a.priority - b.priority);
    }

    dequeue() {
        return this.elements.shift().element;
    }

    isEmpty() {
        return this.elements.length === 0;
    }
}

function solveMaze(depth, targetX, targetY) {
    const erosionLevels = new Map();
    const regionTypes = new Map();

    function getErosionLevel(x, y) {
        const key = `${x},${y}`;
        if (erosionLevels.has(key)) return erosionLevels.get(key);

        let geologicIndex;
        if ((x === 0 && y === 0) || (x === targetX && y === targetY)) {
            geologicIndex = 0;
        } else if (y === 0) {
            geologicIndex = x * 16807;
        } else if (x === 0) {
            geologicIndex = y * 48271;
        } else {
            geologicIndex = getErosionLevel(x-1, y) * getErosionLevel(x, y-1);
        }

        const erosionLevel = (geologicIndex + depth) % 20183;
        erosionLevels.set(key, erosionLevel);
        return erosionLevel;
    }

    function getRegionType(x, y) {
        const key = `${x},${y}`;
        if (regionTypes.has(key)) return regionTypes.get(key);

        const type = getErosionLevel(x, y) % 3;
        regionTypes.set(key, type);
        return type;
    }

    // Part 1
    let riskLevel = 0;
    for (let y = 0; y <= targetY; y++) {
        for (let x = 0; x <= targetX; x++) {
            riskLevel += getRegionType(x, y);
        }
    }

    // Part 2
    const tools = ['torch', 'climbing gear', 'neither'];
    const validTools = [
        ['climbing gear', 'torch'],  // rocky
        ['climbing gear', 'neither'],  // wet
        ['torch', 'neither']  // narrow
    ];

    function heuristic(x, y) {
        return Math.abs(targetX - x) + Math.abs(targetY - y);
    }

    function getNeighbors(x, y, tool) {
        const neighbors = [];
        const directions = [[0, 1], [1, 0], [0, -1], [-1, 0]];
        for (const [dx, dy] of directions) {
            const newX = x + dx;
            const newY = y + dy;
            if (newX >= 0 && newY >= 0) {
                const newType = getRegionType(newX, newY);
                if (validTools[newType].includes(tool)) {
                    neighbors.push([newX, newY, tool, 1]);
                }
            }
        }
        const currentType = getRegionType(x, y);
        for (const newTool of validTools[currentType]) {
            if (newTool !== tool) {
                neighbors.push([x, y, newTool, 7]);
            }
        }
        return neighbors;
    }

    const queue = new PriorityQueue();
    queue.enqueue([0, 0, 'torch', 0], 0);
    const visited = new Set();

    while (!queue.isEmpty()) {
        const [x, y, tool, time] = queue.dequeue();

        if (x === targetX && y === targetY && tool === 'torch') {
            return [riskLevel, time];
        }

        const key = `${x},${y},${tool}`;
        if (visited.has(key)) continue;
        visited.add(key);

        for (const [newX, newY, newTool, timeCost] of getNeighbors(x, y, tool)) {
            const newTime = time + timeCost;
            const priority = newTime + heuristic(newX, newY);
            queue.enqueue([newX, newY, newTool, newTime], priority);
        }
    }
}

// Example usage
const [riskLevel, minTime] = solveMaze(510, 10, 10);
console.log('Risk Level:', riskLevel);
console.log('Minimum Time:', minTime);
