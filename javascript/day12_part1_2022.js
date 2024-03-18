const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');

const start = { x: 0, y: 0 };
const end = { x: 0, y: 0 };
const grid = [];

for (let y = 0; y < input.length; y++) {
  grid[y] = [];
  for (let x = 0; x < input[y].length; x++) {
    if (input[y][x] === 'S') {
      start.x = x;
      start.y = y;
      grid[y][x] = 'a';
    } else if (input[y][x] === 'E') {
      end.x = x;
      end.y = y;
      grid[y][x] = 'z';
    } else {
      grid[y][x] = input[y][x];
    }
  }
}

const queue = [[start.x, start.y, 0]];
const visited = new Set();

while (queue.length) {
  const [x, y, steps] = queue.shift();
  const key = `${x},${y}`;
  if (visited.has(key)) continue;
  visited.add(key);
  if (x === end.x && y === end.y) {
    console.log(steps);
    return;
  }
  for (const [dx, dy] of [[0, 1], [0, -1], [1, 0], [-1, 0]]) {
    const nx = x + dx;
    const ny = y + dy;
    if (nx >= 0 && nx < grid[0].length && ny >= 0 && ny < grid.length && grid[ny][nx].charCodeAt(0) - grid[y][x].charCodeAt(0) <= 1) {
      queue.push([nx, ny, steps + 1]);
    }
  }
}