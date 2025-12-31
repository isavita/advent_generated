
const fs = require('fs');

function main() {
  const data = fs.readFileSync('input.txt', 'utf8')
                 .split('\n')
                 .filter(l => l.length);
  if (!data.length) { console.log('Empty grid'); return; }

  const height = data.length;
  const width = data[0].length;

  let startX = -1, startY = -1, found = false;
  for (let y = 0; y < height && !found; ++y) {
    for (let x = 0; x < width && !found; ++x) {
      if (data[y][x] === 'S') { startX = x; startY = y; found = true; }
    }
  }

  let active = new Set([startX]);
  let splits = 0;

  for (let y = startY; y < height && active.size; ++y) {
    const next = new Set();
    for (const x of active) {
      if (x < 0 || x >= width) continue;
      const ch = data[y][x];
      if (ch === '^') {
        ++splits;
        if (x > 0) next.add(x - 1);
        if (x + 1 < width) next.add(x + 1);
      } else {
        next.add(x);
      }
    }
    active = next;
  }

  console.log(`Total times the beam is split: ${splits}`);
}

main();
