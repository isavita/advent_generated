const fs = require('fs');
const input = fs.readFileSync('input.txt', 'utf8').split('\n');

const grid = {};
const visible = {};

const Neighbors4 = [{ x: 0, y: 1 }, { x: 0, y: -1 }, { x: 1, y: 0 }, { x: -1, y: 0 }];

for (let y = 0; y < input.length; y++) {
  for (let x = 0; x < input[y].length; x++) {
    grid[`${x},${y}`] = parseInt(input[y][x]);
  }
}

for (const p in grid) {
  const [x, y] = p.split(',').map(Number);
  for (const n of Neighbors4) {
    let nextX = x;
    let nextY = y;
    while (true) {
      nextX += n.x;
      nextY += n.y;
      const nextP = `${nextX},${nextY}`;
      if (grid[nextP] !== undefined) {
        if (grid[nextP] >= grid[p]) break;
      } else {
        visible[p] = true;
        break;
      }
    }
  }
}

console.log(Object.keys(visible).length);