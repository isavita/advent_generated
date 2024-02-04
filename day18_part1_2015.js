const fs = require('fs');

const gridSize = 100;
const steps = 100;

function countOnNeighbors(grid, x, y) {
    let on = 0;
    for (let dx = -1; dx <= 1; dx++) {
        for (let dy = -1; dy <= 1; dy++) {
            if (dx === 0 && dy === 0) {
                continue;
            }
            let nx = x + dx;
            let ny = y + dy;
            if (nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize && grid[nx][ny]) {
                on++;
            }
        }
    }
    return on;
}

function step(grid) {
    let newGrid = new Array(gridSize).fill(null).map(() => new Array(gridSize).fill(false));

    for (let x = 0; x < gridSize; x++) {
        for (let y = 0; y < gridSize; y++) {
            let onNeighbors = countOnNeighbors(grid, x, y);
            if (grid[x][y]) {
                newGrid[x][y] = onNeighbors === 2 || onNeighbors === 3;
            } else {
                newGrid[x][y] = onNeighbors === 3;
            }
        }
    }

    return newGrid;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error("Error reading file:", err);
        return;
    }

    let grid = new Array(gridSize).fill(null).map(() => new Array(gridSize).fill(false));

    let lines = data.trim().split('\n');
    let y = 0;
    lines.forEach((line) => {
        for (let x = 0; x < line.length; x++) {
            grid[x][y] = line[x] === '#';
        }
        y++;
    });

    for (let i = 0; i < steps; i++) {
        grid = step(grid);
    }

    let onCount = 0;
    grid.forEach((row) => {
        row.forEach((light) => {
            if (light) {
                onCount++;
            }
        });
    });

    console.log(onCount);
});