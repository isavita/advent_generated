
const fs = require('node:fs');

function solve() {
  const input = fs.readFileSync('input.txt', 'utf8');
  const grid = input.trim().split('\n').map(line => line.split(''));
  const h = grid.length;
  const w = grid[0].length;

  let x, y, dirX, dirY, dirIdx;
  for (let i = 0; i < h; i++) {
    for (let j = 0; j < w; j++) {
      switch (grid[i][j]) {
        case '^': x = j; y = i; dirIdx = 0; break;
        case '>': x = j; y = i; dirIdx = 1; break;
        case 'v': x = j; y = i; dirIdx = 2; break;
        case '<': x = j; y = i; dirIdx = 3; break;
      }
    }
  }

  const dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]];
  dirX = dirs[dirIdx][0];
  dirY = dirs[dirIdx][1];

  const visited = new Set();
  visited.add(`${x},${y}`);

  while (true) {
    const nx = x + dirX;
    const ny = y + dirY;
    if (nx < 0 || nx >= w || ny < 0 || ny >= h) break;
    if (grid[ny][nx] === '#') {
      dirIdx = (dirIdx + 1) % 4;
      dirX = dirs[dirIdx][0];
      dirY = dirs[dirIdx][1];
      continue;
    }
    x = nx;
    y = ny;
    visited.add(`${x},${y}`);
  }

  console.log(visited.size);
}

solve();
