
const fs = require('fs');

function gcd(a, b) {
  if (b === 0) return Math.abs(a);
  return gcd(b, a % b);
}

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

  const linesPerFreq = {};
  for (const f in antennas) {
    linesPerFreq[f] = new Set();
    const coords = antennas[f];
    const n = coords.length;
    for (let i = 0; i < n; i++) {
      for (let j = i + 1; j < n; j++) {
        const [ay, ax] = coords[i];
        const [by, bx] = coords[j];
        let dy = by - ay;
        let dx = bx - ax;
        const g = gcd(dy, dx);
        let sy = dy / g;
        let sx = dx / g;
        if (sx < 0 || (sx === 0 && sy < 0)) {
          sx = -sx;
          sy = -sy;
        }
        const c = sy * ax - sx * ay;
        const key = `${sx},${sy},${c}`;
        linesPerFreq[f].add(key);
      }
    }
  }

  const antinodes = new Set();
  for (const lines of Object.values(linesPerFreq)) {
    for (const key of lines) {
      const [sx, sy, c] = key.split(',').map(Number);
      if (sx === 0 && sy === 0) continue;

      if (sy === 0) {
        if (c % sx === 0) {
          const y = -c / sx;
          if (y >= 0 && y < h) {
            for (let x = 0; x < w; x++) {
              antinodes.add(`${y},${x}`);
            }
          }
        }
      } else if (sx === 0) {
        if (c % sy === 0) {
          const x = c / sy;
          if (x >= 0 && x < w) {
            for (let y = 0; y < h; y++) {
              antinodes.add(`${y},${x}`);
            }
          }
        }
      } else {
        for (let y = 0; y < h; y++) {
          const val = c + sx * y;
          if (val % sy === 0) {
            const x = val / sy;
            if (x >= 0 && x < w) {
              antinodes.add(`${y},${x}`);
            }
          }
        }
      }
    }
  }

  console.log(antinodes.size);
}

solve();
