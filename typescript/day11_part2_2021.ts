const fs = require('fs');

function readInput(filename: string): number[][] {
  const data = fs.readFileSync(filename, 'utf8');
  return data.split('\n').map((line: string) => line.split('').map(Number));
}

function simulateStep(grid: number[][]): number {
  let flashes = 0;
  const flashed: Set<string> = new Set();

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

function flash(grid: number[][], x: number, y: number, flashed: Set<string>): number {
  if (flashed.has(`${x},${y}`)) {
    return 0;
  }

  flashed.add(`${x},${y}`);
  let flashes = 1;
  const directions = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]];

  for (const dir of directions) {
    const newX = x + dir[0];
    const newY = y + dir[1];
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