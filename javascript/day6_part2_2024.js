
const fs = require('fs');

const solve = () => {
  const input = fs.readFileSync('input.txt', 'utf-8').trim();
  const grid = input.split('\n').map(line => line.split(''));
  const h = grid.length;
  const w = grid[0].length;

  let startX, startY, startDir;
  for (let i = 0; i < h; i++) {
    for (let j = 0; j < w; j++) {
      switch (grid[i][j]) {
        case '^':
          startX = j;
          startY = i;
          startDir = 0;
          break;
        case '>':
          startX = j;
          startY = i;
          startDir = 1;
          break;
        case 'v':
          startX = j;
          startY = i;
          startDir = 2;
          break;
        case '<':
          startX = j;
          startY = i;
          startDir = 3;
          break;
      }
    }
  }
  grid[startY][startX] = '.';

  let canLoop = 0;
  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      if (x === startX && y === startY) continue;
      if (grid[y][x] !== '.') continue;
      grid[y][x] = '#';
      if (loops(grid, startX, startY, startDir)) canLoop++;
      grid[y][x] = '.';
    }
  }
  console.log(canLoop);
};

const loops = (grid, sx, sy, sdir) => {
  const h = grid.length;
  const w = grid[0].length;
  const dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]];
  let x = sx, y = sy, dir = sdir;
  const seen = new Set();
  for (let step = 0; step < 2000000; step++) {
    const st = `${x},${y},${dir}`;
    if (seen.has(st)) return true;
    seen.add(st);
    const [dx, dy] = dirs[dir];
    const nx = x + dx;
    const ny = y + dy;
    if (nx < 0 || nx >= w || ny < 0 || ny >= h) return false;
    if (grid[ny][nx] === '#') {
      dir = (dir + 1) % 4;
      continue;
    }
    x = nx;
    y = ny;
  }
  return false;
};

solve();
