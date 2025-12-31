
const fs = require('fs');

function main() {
  const data = fs.readFileSync('input.txt', 'utf8')
    .split('\n')
    .filter(l => l.length);
  if (!data.length) {
    console.log('Total rolls removed: 0');
    return;
  }
  const rows = data.length;
  const cols = data[0].length;
  const grid = data.map(l => l.split(''));
  const dirs = [
    [-1, -1], [-1, 0], [-1, 1],
    [0, -1],          [0, 1],
    [1, -1],  [1, 0], [1, 1]
  ];
  let total = 0;
  while (true) {
    const toRemove = [];
    for (let r = 0; r < rows; r++) {
      for (let c = 0; c < cols; c++) {
        if (grid[r][c] !== '@') continue;
        let cnt = 0;
        for (const [dr, dc] of dirs) {
          const nr = r + dr, nc = c + dc;
          if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] === '@') cnt++;
        }
        if (cnt < 4) toRemove.push([r, c]);
      }
    }
    if (!toRemove.length) break;
    total += toRemove.length;
    for (const [r, c] of toRemove) grid[r][c] = '.';
  }
  console.log(`Total rolls removed: ${total}`);
}

main();
