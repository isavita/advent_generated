const fs = require('fs');
const path = require('path');

// Priority Queue implementation using Min Heap
class PriorityQueue {
    constructor() {
        this.heap = [];
    }

    enqueue(element, priority) {
        this.heap.push({ element, priority });
        this.bubbleUp(this.heap.length - 1);
    }

    dequeue() {
        const min = this.heap[0];
        const end = this.heap.pop();
        if (this.heap.length > 0) {
            this.heap[0] = end;
            this.bubbleDown(0);
        }
        return min;
    }

    bubbleUp(index) {
        const element = this.heap[index];
        while (index > 0) {
            const parentIdx = Math.floor((index - 1) / 2);
            const parent = this.heap[parentIdx];
            if (element.priority >= parent.priority) break;
            this.heap[parentIdx] = element;
            this.heap[index] = parent;
            index = parentIdx;
        }
    }

    bubbleDown(index) {
        const length = this.heap.length;
        const element = this.heap[index];
        while (true) {
            let leftIdx = 2 * index + 1;
            let rightIdx = 2 * index + 2;
            let swap = null;

            if (leftIdx < length) {
                if (this.heap[leftIdx].priority < element.priority) {
                    swap = leftIdx;
                }
            }

            if (rightIdx < length) {
                if (
                    (swap === null && this.heap[rightIdx].priority < element.priority) ||
                    (swap !== null && this.heap[rightIdx].priority < this.heap[leftIdx].priority)
                ) {
                    swap = rightIdx;
                }
            }

            if (swap === null) break;
            this.heap[index] = this.heap[swap];
            this.heap[swap] = element;
            index = swap;
        }
    }

    isEmpty() {
        return this.heap.length === 0;
    }
}

// Function to read the input map
function readMap(filePath) {
    const data = fs.readFileSync(filePath, 'utf-8');
    const map = data.trim().split('\n').map(line => line.split(''));
    return map;
}

// Function to modify the map for Part Two
function modifyMapForPartTwo(map) {
    // Find the center
    let centerY = Math.floor(map.length / 2);
    let centerX = Math.floor(map[0].length / 2);
    // Replace the center 3x3 with walls and four starting positions
    for (let y = centerY - 1; y <= centerY + 1; y++) {
        for (let x = centerX - 1; x <= centerX + 1; x++) {
            if (y === centerY && x === centerX) {
                map[y][x] = '#';
            } else if ((y === centerY - 1 || y === centerY + 1) && (x === centerX - 1 || x === centerX + 1)) {
                map[y][x] = '#';
            } else {
                map[y][x] = '#';
            }
        }
    }
    // Place four starting positions
    map[centerY - 1][centerX - 1] = '@';
    map[centerY - 1][centerX + 1] = '@';
    map[centerY + 1][centerX - 1] = '@';
    map[centerY + 1][centerX + 1] = '@';
    return map;
}

// Function to find all keys and starting positions
function findKeysAndEntrances(map) {
    const keys = {};
    const doors = {};
    const entrances = [];
    for (let y = 0; y < map.length; y++) {
        for (let x = 0; x < map[0].length; x++) {
            const char = map[y][x];
            if (char >= 'a' && char <= 'z') {
                keys[char] = { y, x };
            } else if (char >= 'A' && char <= 'Z') {
                doors[char] = { y, x };
            } else if (char === '@') {
                entrances.push({ y, x });
            }
        }
    }
    return { keys, doors, entrances };
}

// Function to compute all reachable keys from a position with current keys
function getAllReachableKeys(map, startY, startX, collectedKeys) {
    const queue = [];
    const visited = Array(map.length).fill(0).map(() => Array(map[0].length).fill(false));
    queue.push({ y: startY, x: startX, steps: 0, keys: new Set() });
    visited[startY][startX] = true;
    const reachable = [];

    while (queue.length > 0) {
        const current = queue.shift();
        const { y, x, steps, keys } = current;

        const directions = [
            { dy: -1, dx: 0 },
            { dy: 1, dx: 0 },
            { dy: 0, dx: -1 },
            { dy: 0, dx: 1 },
        ];

        for (const dir of directions) {
            const ny = y + dir.dy;
            const nx = x + dir.dx;

            if (
                ny < 0 ||
                ny >= map.length ||
                nx < 0 ||
                nx >= map[0].length ||
                visited[ny][nx]
            ) {
                continue;
            }

            const cell = map[ny][nx];
            if (cell === '#') continue;

            // If it's a door, check if the key is collected
            if (cell >= 'A' && cell <= 'Z' && !collectedKeys.has(cell.toLowerCase())) {
                continue;
            }

            const newKeys = new Set(keys);
            if (cell >= 'a' && cell <= 'z') {
                if (!collectedKeys.has(cell)) {
                    newKeys.add(cell);
                    reachable.push({ key: cell, steps: steps + 1 });
                }
            }

            visited[ny][nx] = true;
            queue.push({ y: ny, x: nx, steps: steps + 1, keys: newKeys });
        }
    }

    return reachable;
}

// Function to build a graph of keys and their reachable keys
function buildGraph(map, keys, entrances, collectedKeys) {
    const graph = {};

    const allPoints = { ...keys };
    entrances.forEach((entrance, idx) => {
        allPoints[`entrance_${idx}`] = entrance;
    });

    for (const [key, pos] of Object.entries(allPoints)) {
        graph[key] = getAllReachableKeys(map, pos.y, pos.x, collectedKeys);
    }

    return graph;
}

// Main function to solve the problem
function solve(map) {
    const { keys, doors, entrances } = findKeysAndEntrances(map);
    const totalKeys = Object.keys(keys).length;

    // Precompute the graph
    const graph = buildGraph(map, keys, entrances, new Set());

    // Initial state: positions of all robots and no keys collected
    const initialPositions = entrances.map(e => `${e.y},${e.x}`);
    const initialState = {
        positions: initialPositions,
        keys: new Set(),
        steps: 0,
    };

    // Priority Queue for BFS
    const pq = new PriorityQueue();
    pq.enqueue(initialState, 0);

    // Visited states: Map with key as positions + keys
    const visited = new Set();

    while (!pq.isEmpty()) {
        const current = pq.dequeue().element;
        const { positions, keys: collectedKeys, steps } = current;

        // Check if all keys are collected
        if (collectedKeys.size === totalKeys) {
            console.log(`Fewest steps to collect all keys: ${steps}`);
            return;
        }

        // Generate a unique key for the current state
        const stateKey = positions.join(';') + ';' + Array.from(collectedKeys).sort().join('');
        if (visited.has(stateKey)) continue;
        visited.add(stateKey);

        // For each robot, find reachable keys
        for (let i = 0; i < positions.length; i++) {
            const [y, x] = positions[i].split(',').map(Number);
            const reachable = getAllReachableKeys(map, y, x, collectedKeys);

            for (const { key, steps: stepCount } of reachable) {
                if (collectedKeys.has(key)) continue;

                // Collect the key
                const newKeys = new Set(collectedKeys);
                newKeys.add(key);

                // Update positions
                const newPositions = [...positions];
                newPositions[i] = `${keys[key].y},${keys[key].x}`;

                // Create new state
                const newState = {
                    positions: newPositions,
                    keys: newKeys,
                    steps: steps + stepCount,
                };

                // Enqueue the new state
                pq.enqueue(newState, steps + stepCount);
            }
        }
    }

    console.log("No solution found.");
}

// Enhanced functions to improve performance and handle multiple robots

// Function to compute all reachable keys and their paths from a position
function bfs(map, startY, startX) {
    const queue = [];
    const visited = Array(map.length).fill(0).map(() => Array(map[0].length).fill(false));
    queue.push({ y: startY, x: startX, steps: 0, doors: new Set() });
    visited[startY][startX] = true;
    const reachable = {};

    while (queue.length > 0) {
        const current = queue.shift();
        const { y, x, steps, doors } = current;

        const directions = [
            { dy: -1, dx: 0 },
            { dy: 1, dx: 0 },
            { dy: 0, dx: -1 },
            { dy: 0, dx: 1 },
        ];

        for (const dir of directions) {
            const ny = y + dir.dy;
            const nx = x + dir.dx;

            if (
                ny < 0 ||
                ny >= map.length ||
                nx < 0 ||
                nx >= map[0].length ||
                visited[ny][nx]
            ) {
                continue;
            }

            const cell = map[ny][nx];
            if (cell === '#') continue;

            let newDoors = new Set(doors);
            if (cell >= 'A' && cell <= 'Z') {
                newDoors.add(cell.toLowerCase());
            }

            if (cell >= 'a' && cell <= 'z') {
                reachable[cell] = { steps: steps + 1, doors: new Set(newDoors) };
            }

            visited[ny][nx] = true;
            queue.push({ y: ny, x: nx, steps: steps + 1, doors: newDoors });
        }
    }

    return reachable;
}

// Function to build a complete graph of all keys and entrances
function buildCompleteGraph(map, keys, entrances) {
    const graph = {};

    const allPoints = { ...keys };
    entrances.forEach((entrance, idx) => {
        allPoints[`entrance_${idx}`] = entrance;
    });

    for (const [key, pos] of Object.entries(allPoints)) {
        graph[key] = bfs(map, pos.y, pos.x);
    }

    return graph;
}

// Optimized BFS with multiple robots
function optimizedSolve(map) {
    const { keys, doors, entrances } = findKeysAndEntrances(map);
    const totalKeys = Object.keys(keys).length;

    // Build complete graph
    const graph = buildCompleteGraph(map, keys, entrances);

    // Precompute all keys bitmask
    let allKeysBitmask = 0;
    const keyToBit = {};
    let bit = 0;
    for (const key of Object.keys(keys)) {
        keyToBit[key] = bit;
        allKeysBitmask |= (1 << bit);
        bit++;
    }

    // Initial state: positions of all robots and no keys collected
    const initialPositions = entrances.map((e, idx) => `entrance_${idx}`);
    const initialState = {
        robots: initialPositions,
        keys: 0,
        steps: 0,
    };

    // Priority Queue for BFS
    const pq = new PriorityQueue();
    pq.enqueue(initialState, 0);

    // Visited states: Map with key as robots' positions and keys
    const visited = new Set();

    while (!pq.isEmpty()) {
        const current = pq.dequeue().element;
        const { robots, keys: collectedKeys, steps } = current;

        // Check if all keys are collected
        if (collectedKeys === allKeysBitmask) {
            console.log(`Fewest steps to collect all keys: ${steps}`);
            return;
        }

        // Generate a unique key for the current state
        const stateKey = robots.join(';') + ';' + collectedKeys;
        if (visited.has(stateKey)) continue;
        visited.add(stateKey);

        // For each robot, find reachable keys
        for (let i = 0; i < robots.length; i++) {
            const currentPos = robots[i];
            const reachable = graph[currentPos];

            for (const [key, info] of Object.entries(reachable)) {
                const keyBit = keyToBit[key];
                if (keyBit === undefined) continue;
                if ((collectedKeys & (1 << keyBit)) !== 0) continue;

                // Check if all doors required for this key are opened
                let canAccess = true;
                for (const door of info.doors) {
                    const doorBit = keyToBit[door];
                    if (doorBit === undefined || (collectedKeys & (1 << doorBit)) === 0) {
                        canAccess = false;
                        break;
                    }
                }
                if (!canAccess) continue;

                // New state
                const newRobots = [...robots];
                newRobots[i] = key;
                const newKeys = collectedKeys | (1 << keyBit);
                const newSteps = steps + info.steps;

                const newState = {
                    robots: newRobots,
                    keys: newKeys,
                    steps: newSteps,
                };

                pq.enqueue(newState, newSteps);
            }
        }
    }

    console.log("No solution found.");
}

// Main execution
(function main() {
    const inputPath = path.join(__dirname, 'input.txt');
    let map = readMap(inputPath);
    map = modifyMapForPartTwo(map);
    optimizedSolve(map);
})();
