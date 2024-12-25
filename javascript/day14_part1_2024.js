
const fs = require('node:fs');

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
const width = 101;
const height = 103;
const robots = input.map(line => {
  const [pPart, vPart] = line.split(' ');
  const [px, py] = pPart.slice(2).split(',').map(Number);
  const [vx, vy] = vPart.slice(2).split(',').map(Number);
  return [px, py, vx, vy];
});

for (let i = 0; i < 100; i++) {
  for (const r of robots) {
    r[0] = (r[0] + r[2]) % width;
    r[1] = (r[1] + r[3]) % height;
    if (r[0] < 0) r[0] += width;
    if (r[1] < 0) r[1] += height;
  }
}

let q1 = 0, q2 = 0, q3 = 0, q4 = 0;
for (const r of robots) {
  const [x, y] = r;
  if (x === 50 || y === 51) continue;
  if (x < 50 && y < 51) q1++;
  else if (x > 50 && y < 51) q2++;
  else if (x < 50 && y > 51) q3++;
  else q4++;
}

console.log(q1 * q2 * q3 * q4);
