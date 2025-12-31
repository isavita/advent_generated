
const fs = require('fs');

function main() {
  const data = fs.readFileSync('input.txt', 'utf8').trimEnd();
  if (!data) {
    console.log('Empty grid');
    return;
  }
  const lines = data.split(/\r?\n/).filter(l => l.length);
  const rows = lines.length;
  const cols = lines[0].length;
  const grid = lines.map(l => l.split(''));
  const dirs = [-1, 0, 1];
  let count = 0;
  for (let y = 0; y < rows; y++) {
    for (let x = 0; x < cols; x++) {
      if (grid[y][x] !== '@') continue;
      let neigh = 0;
      for (let dy of dirs) {
        for (let dx of dirs) {
          if (dx === 0 && dy === 0) continue;
          const ny = y + dy, nx = x + dx;
          if (ny >= 0 && ny < rows && nx >= 0 && nx < cols && grid[ny][nx] === '@') {
            neigh++;
            if (neigh >= 4) break;
          }
        }
        if (neigh >= 4) break;
      }
      if (neigh < 4) count++;
    }
  }
  console.log(`Number of accessible rolls of paper: ${count}`);
}

main();
