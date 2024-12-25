
function gcd(a: number, b: number): number {
  return b === 0 ? (a < 0 ? -a : a) : gcd(b, a % b);
}

const fs = require('fs');
const grid = fs.readFileSync('input.txt', 'utf-8').split('\n').filter((line: string) => line.length > 0);
const h = grid.length;
const w = grid[0].length;
const antennas: { [key: string]: [number, number][] } = {};

for (let y = 0; y < h; y++) {
  for (let x = 0; x < w; x++) {
    const c = grid[y][x];
    if (c !== '.') {
      antennas[c] = (antennas[c] || []).concat([[y, x]]);
    }
  }
}

const linesPerFreq: { [key: string]: { [key: string]: boolean } } = {};
for (const f in antennas) {
  linesPerFreq[f] = {};
  const coords = antennas[f];
  const n = coords.length;
  for (let i = 0; i < n; i++) {
    for (let j = i + 1; j < n; j++) {
      const A = coords[i];
      const B = coords[j];
      let dy = B[0] - A[0];
      let dx = B[1] - A[1];
      const g = gcd(dy, dx);
      let sy = dy / g;
      let sx = dx / g;
      if (sx < 0 || (sx === 0 && sy < 0)) {
        sx = -sx;
        sy = -sy;
      }
      const c = sy * A[1] - sx * A[0];
      const key = `${sx},${sy},${c}`;
      linesPerFreq[f][key] = true;
    }
  }
}

const antinodes = new Set<string>();
for (const freq in linesPerFreq) {
  for (const key in linesPerFreq[freq]) {
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
