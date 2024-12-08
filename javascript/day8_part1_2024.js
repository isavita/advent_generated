
const fs = require('fs');

function solve() {
  const grid = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
  const h = grid.length;
  const w = grid[0].length;
  const antennas = {};

  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      const c = grid[y][x];
      if (c !== '.') {
        antennas[c] = antennas[c] || [];
        antennas[c].push([y, x]);
      }
    }
  }

  const antinodes = new Set();
  for (const coords of Object.values(antennas)) {
    const n = coords.length;
    for (let i = 0; i < n; i++) {
      for (let j = i + 1; j < n; j++) {
        const [ay, ax] = coords[i];
        const [by, bx] = coords[j];
        const p1 = [2 * ay - by, 2 * ax - bx];
        const p2 = [2 * by - ay, 2 * bx - ax];

        if (p1[0] >= 0 && p1[0] < h && p1[1] >= 0 && p1[1] < w) {
          antinodes.add(p1.toString());
        }
        if (p2[0] >= 0 && p2[0] < h && p2[1] >= 0 && p2[1] < w) {
          antinodes.add(p2.toString());
        }
      }
    }
  }
  console.log(antinodes.size);
}

solve();
