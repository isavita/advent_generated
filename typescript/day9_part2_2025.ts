
import { readFileSync } from 'fs';

type Point = { x: number; y: number };

const data = readFileSync('input.txt', 'utf8')
  .trim()
  .split('\n')
  .filter(l => l.trim())
  .map(l => {
    const [a, b] = l.split(',').map(s => Number(s.trim()));
    return { x: a, y: b } as Point;
  });

if (!data.length) process.exit(0);

const xs = Array.from(new Set(data.map(p => p.x))).sort((a, b) => a - b);
const ys = Array.from(new Set(data.map(p => p.y))).sort((a, b) => a - b);
const xIdx = new Map<number, number>(xs.map((v, i) => [v, i]));
const yIdx = new Map<number, number>(ys.map((v, i) => [v, i]));

const w = 2 * xs.length - 1;
const h = 2 * ys.length - 1;
const grid = Array.from({ length: w }, () => new Uint8Array(h));

for (let i = 0; i < data.length; i++) {
  const a = data[i];
  const b = data[(i + 1) % data.length];
  const ix1 = xIdx.get(a.x)!, iy1 = yIdx.get(a.y)!;
  const ix2 = xIdx.get(b.x)!, iy2 = yIdx.get(b.y)!;
  const x0 = Math.min(ix1, ix2) * 2;
  const x1 = Math.max(ix1, ix2) * 2;
  const y0 = Math.min(iy1, iy2) * 2;
  const y1 = Math.max(iy1, iy2) * 2;
  for (let x = x0; x <= x1; x++) for (let y = y0; y <= y1; y++) grid[x][y] = 1;
}

for (let i = 0; i < xs.length - 1; i++) {
  const midX = (xs[i] + xs[i + 1]) / 2;
  const cross: number[] = [];
  for (let k = 0; k < data.length; k++) {
    const a = data[k];
    const b = data[(k + 1) % data.length];
    if (a.y === b.y && ((a.x < midX && midX < b.x) || (b.x < midX && midX < a.x))) cross.push(a.y);
  }
  cross.sort((a, b) => a - b);
  let inside = false, idx = 0;
  for (let j = 0; j < ys.length - 1; j++) {
    const midY = (ys[j] + ys[j + 1]) / 2;
    while (idx < cross.length && cross[idx] < midY) { inside = !inside; idx++; }
    if (inside) {
      for (let gx = i * 2; gx <= i * 2 + 2; gx++) {
        for (let gy = j * 2; gy <= j * 2 + 2; gy++) grid[gx][gy] = 1;
      }
    }
  }
}

const pref = Array.from({ length: w + 1 }, () => new Uint32Array(h + 1));
for (let i = 0; i < w; i++) {
  for (let j = 0; j < h; j++) {
    pref[i + 1][j + 1] = (grid[i][j] ? 1 : 0) + pref[i][j + 1] + pref[i + 1][j] - pref[i][j];
  }
}

let maxArea = 0;
for (let i = 0; i < data.length; i++) {
  for (let j = 0; j < data.length; j++) {
    const p1 = data[i], p2 = data[j];
    const gx1 = xIdx.get(p1.x)! * 2, gy1 = yIdx.get(p1.y)! * 2;
    const gx2 = xIdx.get(p2.x)! * 2, gy2 = yIdx.get(p2.y)! * 2;
    const minX = Math.min(gx1, gx2), maxX = Math.max(gx1, gx2);
    const minY = Math.min(gy1, gy2), maxY = Math.max(gy1, gy2);
    const needed = (maxX - minX + 1) * (maxY - minY + 1);
    const actual = pref[maxX + 1][maxY + 1] - pref[minX][maxY + 1] - pref[maxX + 1][minY] + pref[minX][minY];
    if (actual === needed) {
      const area = (Math.abs(p1.x - p2.x) + 1) * (Math.abs(p1.y - p2.y) + 1);
      if (area > maxArea) maxArea = area;
    }
  }
}

console.log(maxArea);
