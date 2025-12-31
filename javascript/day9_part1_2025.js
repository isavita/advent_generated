
const fs = require('fs');

const lines = fs.readFileSync('input.txt', 'utf8').trim().split(/\r?\n/);
const xs = [];
const ys = [];

for (const line of lines) {
  if (!line) continue;
  const parts = line.split(',');
  if (parts.length !== 2) continue;
  const x = parseInt(parts[0].trim(), 10);
  const y = parseInt(parts[1].trim(), 10);
  if (isNaN(x) || isNaN(y)) continue;
  xs.push(x);
  ys.push(y);
}

const n = xs.length;
let maxArea = n ? 1 : 0;

for (let i = 0; i < n; i++) {
  const x1 = xs[i];
  const y1 = ys[i];
  for (let j = i + 1; j < n; j++) {
    const w = Math.abs(x1 - xs[j]) + 1;
    const h = Math.abs(y1 - ys[j]) + 1;
    const area = w * h;
    if (area > maxArea) maxArea = area;
  }
}

console.log(maxArea);
