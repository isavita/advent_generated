const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const adjacentCounts = (grid, x, y) => {
  const counts = { '.': 0, '|': 0, '#': 0 };
  for (let dx = -1; dx <= 1; dx++) {
    for (let dy = -1; dy <= 1; dy++) {
      if (dx === 0 && dy === 0) continue;
      const nx = x + dx;
      const ny = y + dy;
      if (nx >= 0 && nx < grid[0].length && ny >= 0 && ny < grid.length) {
        counts[grid[ny][nx]]++;
      }
    }
  }
  return counts;
};

const transform = (grid) => {
  return grid.map((row, y) => {
    return row.split('').map((cell, x) => {
      const { '.': open, '|': trees, '#': lumberyards } = adjacentCounts(grid, x, y);
      switch (cell) {
        case '.':
          return trees >= 3 ? '|' : '.';
        case '|':
          return lumberyards >= 3 ? '#' : '|';
        case '#':
          return lumberyards >= 1 && trees >= 1 ? '#' : '.';
      }
    }).join('');
  });
};

const resourceValue = (grid) => {
  let wooded = 0;
  let lumberyards = 0;
  grid.forEach(row => {
    wooded += row.split('').filter(cell => cell === '|').length;
    lumberyards += row.split('').filter(cell => cell === '#').length;
  });
  return wooded * lumberyards;
};

let current = input.slice();
for (let i = 0; i < 10; i++) {
  current = transform(current);
}

console.log(resourceValue(current));