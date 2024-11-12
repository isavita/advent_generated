// blizzardBasinPartTwo.js

const fs = require('fs');
const path = require('path');

// Function to compute GCD
function gcd(a, b) {
    while (b !== 0) {
        let temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

// Function to compute LCM
function lcm(a, b) {
    return (a * b) / gcd(a, b);
}

// Function to compute positive modulo
function mod(n, m) {
    return ((n % m) + m) % m;
}

// Directions mapping
const directions = {
    '>': { dx: 1, dy: 0 },
    '<': { dx: -1, dy: 0 },
    '^': { dx: 0, dy: -1 },
    'v': { dx: 0, dy: 1 },
};

// Read input.txt
const inputPath = path.join(__dirname, 'input.txt');
const input = fs.readFileSync(inputPath, 'utf-8');
const lines = input.trim().split('\n');

// Parse grid
const grid = lines.map(line => line.split(''));
const height = grid.length - 2; // excluding walls
const width = grid[0].length - 2; // excluding walls

// Find start and goal positions
let start, goal;
for (let x = 0; x < grid[0].length; x++) {
    if (grid[0][x] === '.') {
        start = { x: x - 1, y: -1 }; // y = -1 indicates just above the first row
        break;
    }
}
for (let x = 0; x < grid[grid.length - 1].length; x++) {
    if (grid[grid.length - 1][x] === '.') {
        goal = { x: x - 1, y: height }; // y = height indicates just below the last row
        break;
    }
}

// Find initial blizzards
let blizzards = [];
for (let y = 1; y < grid.length - 1; y++) {
    for (let x = 1; x < grid[0].length - 1; x++) {
        const cell = grid[y][x];
        if (cell in directions) {
            blizzards.push({ x: x - 1, y: y - 1, dir: cell });
        }
    }
}

// Compute period
const period = lcm(width, height);

// Precompute blizzard positions
const blizzardMap = Array.from({ length: period }, () => new Set());

blizzards.forEach(blizzard => {
    const { x, y, dir } = blizzard;
    const { dx, dy } = directions[dir];
    for (let t = 0; t < period; t++) {
        const nx = mod(x + dx * t, width);
        const ny = mod(y + dy * t, height);
        const pos = `${nx},${ny}`;
        blizzardMap[t].add(pos);
    }
});

// BFS Function
function bfs(startPos, goalPos, startTime) {
    const queue = [];
    const visited = new Set();

    queue.push({ x: startPos.x, y: startPos.y, t: startTime });
    visited.add(`${startPos.x},${startPos.y},${startTime % period}`);

    // Possible moves: wait, up, down, left, right
    const moves = [
        { dx: 0, dy: 0 }, // wait
        { dx: 1, dy: 0 }, // right
        { dx: -1, dy: 0 }, // left
        { dx: 0, dy: 1 }, // down
        { dx: 0, dy: -1 }, // up
    ];

    while (queue.length > 0) {
        const current = queue.shift();
        const { x, y, t } = current;

        for (const move of moves) {
            const nx = x + move.dx;
            const ny = y + move.dy;
            const nt = t + 1;

            // Check if reached goal
            if (nx === goalPos.x && ny === goalPos.y) {
                return nt;
            }

            // Check if the new position is the start position
            const isStart = (nx === startPos.x && ny === startPos.y);

            // Check if the new position is the goal position
            const isGoal = (nx === goalPos.x && ny === goalPos.y);

            // Check if the new position is within bounds or is start/goal
            const withinBounds = (nx >= 0 && nx < width && ny >= 0 && ny < height);
            const canStay = withinBounds || isStart || isGoal;

            if (!canStay) {
                continue; // Position is outside the grid and not start or goal
            }

            // Check if the position is a wall
            const isWall =
                (ny === -1 && !(nx === startPos.x && ny === startPos.y)) || // Above the first row only start is allowed
                (ny === height && !(nx === goalPos.x && ny === goalPos.y)) || // Below the last row only goal is allowed
                (nx < 0 || nx >= width || ny < 0 || ny >= height); // Walls

            if (isWall && !(isStart || isGoal)) {
                continue; // Cannot move into a wall
            }

            // Check if the new position is safe (no blizzard)
            const blizzardsAtNextTime = blizzardMap[nt % period];
            const posKey = `${nx},${ny}`;
            if (blizzardsAtNextTime.has(posKey)) {
                continue; // Position has a blizzard
            }

            // Check if this state has been visited
            const stateKey = `${nx},${ny},${nt % period}`;
            if (visited.has(stateKey)) {
                continue; // Already visited this state
            }

            // Mark the state as visited and add to queue
            visited.add(stateKey);
            queue.push({ x: nx, y: ny, t: nt });
        }
    }

    // If goal is not reachable
    return Infinity;
}

// Main Execution

// Define the sequence of trips: Start -> Goal, Goal -> Start, Start -> Goal
const trips = [
    { from: start, to: goal },
    { from: goal, to: start },
    { from: start, to: goal },
];

let currentTime = 0;
let totalTime = 0;

for (let i = 0; i < trips.length; i++) {
    const trip = trips[i];
    const tripTime = bfs(trip.from, trip.to, currentTime);
    if (tripTime === Infinity) {
        console.log("No solution found for trip", i + 1);
        process.exit(0);
    }
    totalTime = tripTime;
    currentTime = tripTime;
}

console.log(totalTime);
