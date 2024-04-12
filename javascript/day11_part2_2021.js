const fs = require('fs');

function readInput(filename) {
  const data = fs.readFileSync(filename, 'utf-8');
  return data.trim().split('\n').map(row => row.split('').map(Number));
}

function simulateStep(grid) {
  let flashes = 0;
  const flashed = new Set();

  for (let y = 0; y < grid.length; y++) {
    for (let x = 0; x < grid[y].length; x++) {
      grid[y][x]++;
    }
  }

  for (let y = 0; y < grid.length; y++) {
    for (let x = 0; x < grid[y].length; x++) {
      if (grid[y][x] > 9) {
        flashes += flash(grid, x, y, flashed);
      }
    }
  }

  for (const coords of flashed) {
    const [x, y] = coords.split(',').map(Number);
    grid[y][x] = 0;
  }

  return flashes;
}

function flash(grid, x, y, flashed) {
  const key = `${x},${y}`;
  if (flashed.has(key)) {
    return 0;
  }

  flashed.add(key);
  let flashes = 1;
  const directions = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]];

  for (const [dx, dy] of directions) {
    const newX = x + dx;
    const newY = y + dy;
    if (newX >= 0 && newX < grid[0].length && newY >= 0 && newY < grid.length) {
      grid[newY][newX]++;
      if (grid[newY][newX] > 9) {
        flashes += flash(grid, newX, newY, flashed);
      }
    }
  }

  return flashes;
}

function main() {
  const grid = readInput('input.txt');

  let step = 0;
  while (true) {
    step++;
    const flashes = simulateStep(grid);
    if (flashes === 100) {
      break;
    }
  }

  console.log(step);
}

main();