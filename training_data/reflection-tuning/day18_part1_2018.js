const fs = require('fs');

const OPEN = '.';
const TREES = '|';
const LUMBERYARD = '#';

function readInput(filename) {
    return fs.readFileSync(filename, 'utf8').trim().split('\n').map(line => line.split(''));
}

function countAdjacent(grid, x, y, type) {
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

function simulateMinute(grid) {
    const newGrid = grid.map(row => [...row]);
    for (let x = 0; x < grid.length; x++) {
        for (let y = 0; y < grid[0].length; y++) {
            const adjacentTrees = countAdjacent(grid, x, y, TREES);
            const adjacentLumberyards = countAdjacent(grid, x, y, LUMBERYARD);
            
            if (grid[x][y] === OPEN && adjacentTrees >= 3) {
                newGrid[x][y] = TREES;
            } else if (grid[x][y] === TREES && adjacentLumberyards >= 3) {
                newGrid[x][y] = LUMBERYARD;
            } else if (grid[x][y] === LUMBERYARD && !(adjacentLumberyards >= 1 && adjacentTrees >= 1)) {
                newGrid[x][y] = OPEN;
            }
        }
    }
    return newGrid;
}

function countResources(grid) {
    let trees = 0, lumberyards = 0;
    for (let row of grid) {
        for (let acre of row) {
            if (acre === TREES) trees++;
            else if (acre === LUMBERYARD) lumberyards++;
        }
    }
    return trees * lumberyards;
}

function solve() {
    let grid = readInput('input.txt');
    for (let i = 0; i < 10; i++) {
        grid = simulateMinute(grid);
    }
    return countResources(grid);
}

console.log(solve());
