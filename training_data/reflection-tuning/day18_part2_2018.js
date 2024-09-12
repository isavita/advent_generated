const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
let grid = input.map(line => line.split(''));

function countAdjacent(x, y, type) {
    let count = 0;
    for (let dx = -1; dx <= 1; dx++) {
        for (let dy = -1; dy <= 1; dy++) {
            if (dx === 0 && dy === 0) continue;
            const nx = x + dx, ny = y + dy;
            if (nx >= 0 && nx < grid.length && ny >= 0 && ny < grid[0].length && grid[nx][ny] === type) {
                count++;
            }
        }
    }
    return count;
}

function simulate() {
    const newGrid = grid.map(row => [...row]);
    for (let x = 0; x < grid.length; x++) {
        for (let y = 0; y < grid[0].length; y++) {
            if (grid[x][y] === '.' && countAdjacent(x, y, '|') >= 3) {
                newGrid[x][y] = '|';
            } else if (grid[x][y] === '|' && countAdjacent(x, y, '#') >= 3) {
                newGrid[x][y] = '#';
            } else if (grid[x][y] === '#' && !(countAdjacent(x, y, '#') >= 1 && countAdjacent(x, y, '|') >= 1)) {
                newGrid[x][y] = '.';
            }
        }
    }
    grid = newGrid;
}

function countResources() {
    let wooded = 0, lumberyards = 0;
    for (let row of grid) {
        for (let cell of row) {
            if (cell === '|') wooded++;
            if (cell === '#') lumberyards++;
        }
    }
    return wooded * lumberyards;
}

// Part 1
for (let i = 0; i < 10; i++) simulate();
console.log("Part 1:", countResources());

// Part 2
const seen = new Map();
let minute = 10;
while (true) {
    simulate();
    minute++;
    const key = grid.map(row => row.join('')).join('');
    if (seen.has(key)) {
        const cycleLength = minute - seen.get(key);
        const remaining = (1000000000 - minute) % cycleLength;
        for (let i = 0; i < remaining; i++) simulate();
        break;
    }
    seen.set(key, minute);
}

console.log("Part 2:", countResources());
