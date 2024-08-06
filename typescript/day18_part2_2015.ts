const fs = require('fs');
const input = fs.readFileSync('input.txt', 'utf8').split('\n');

const gridSize = 100;
const steps = 100;

let grid = Array(gridSize).fill(0).map(() => Array(gridSize).fill(false));

for (let y = 0; y < input.length; y++) {
  for (let x = 0; x < input[y].length; x++) {
    grid[x][y] = input[y][x] === '#';
  }
}

grid[0][0] = true;
grid[0][gridSize-1] = true;
grid[gridSize-1][0] = true;
grid[gridSize-1][gridSize-1] = true;

for (let i = 0; i < steps; i++) {
  grid = step(grid);
}

let onCount = 0;
for (let x = 0; x < gridSize; x++) {
  for (let y = 0; y < gridSize; y++) {
    if (grid[x][y]) {
      onCount++;
    }
  }
}

console.log(onCount);

function step(grid) {
  const newGrid = Array(gridSize).fill(0).map(() => Array(gridSize).fill(false));

  for (let x = 0; x < gridSize; x++) {
    for (let y = 0; y < gridSize; y++) {
      let onNeighbors = 0;
      for (let dx = -1; dx <= 1; dx++) {
        for (let dy = -1; dy <= 1; dy++) {
          if (dx === 0 && dy === 0) {
            continue;
          }
          const nx = x + dx;
          const ny = y + dy;
          if (nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize && grid[nx][ny]) {
            onNeighbors++;
          }
        }
      }
      newGrid[x][y] = (grid[x][y] && (onNeighbors === 2 || onNeighbors === 3)) || (!grid[x][y] && onNeighbors === 3);
    }
  }

  newGrid[0][0] = true;
  newGrid[0][gridSize-1] = true;
  newGrid[gridSize-1][0] = true;
  newGrid[gridSize-1][gridSize-1] = true;

  return newGrid;
}