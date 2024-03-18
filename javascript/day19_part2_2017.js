const fs = require('fs');

const grid = fs.readFileSync('input.txt', 'utf8')
  .split('\n')
  .map(line => line.split(''));

let x, y;
for (let i = 0; i < grid[0].length; i++) {
  if (grid[0][i] === '|') {
    x = i;
    break;
  }
}
y = 0;

let dx = 0, dy = 1;
let steps = 0;

while (x >= 0 && x < grid[0].length && y >= 0 && y < grid.length) {
  if (grid[y][x] === ' ') {
    break;
  }

  steps++;

  if (grid[y][x] === '+') {
    if (dx === 0) {
      if (x > 0 && (grid[y][x-1] === '-' || (grid[y][x-1] >= 'A' && grid[y][x-1] <= 'Z'))) {
        dx = -1, dy = 0;
      } else {
        dx = 1, dy = 0;
      }
    } else {
      if (y > 0 && (grid[y-1][x] === '|' || (grid[y-1][x] >= 'A' && grid[y-1][x] <= 'Z'))) {
        dx = 0, dy = -1;
      } else {
        dx = 0, dy = 1;
      }
    }
  }

  x += dx;
  y += dy;
}

console.log(steps);