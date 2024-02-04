const fs = require("fs");

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

    newGrid[0][0] = true;
    newGrid[0][gridSize - 1] = true;
    newGrid[gridSize - 1][0] = true;
    newGrid[gridSize - 1][gridSize - 1] = true;

    return newGrid;
}

const input = fs.readFileSync("input.txt", "utf8").trim().split("\n");
let grid = new Array(gridSize).fill(null).map(() => new Array(gridSize).fill(false));

let y = 0;
input.forEach((line) => {
    for (let x = 0; x < line.length; x++) {
        grid[x][y] = line[x] === "#";
    }
    y++;
});

grid[0][0] = true;
grid[0][gridSize - 1] = true;
grid[gridSize - 1][0] = true;
grid[gridSize - 1][gridSize - 1] = true;

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