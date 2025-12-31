
const fs = require('fs');
const lines = fs.readFileSync('input.txt', 'utf8')
  .split('\n')
  .filter(l => l.length);

if (!lines.length) {
  console.log('0');
  process.exit(0);
}

const h = lines.length;
const w = lines[0].length;
let sx = -1, sy = -1;
outer:
for (let y = 0; y < h; y++) {
  const row = lines[y];
  for (let x = 0; x < w; x++) {
    if (row[x] === 'S') {
      sx = x;
      sy = y;
      break outer;
    }
  }
}
if (sx === -1) {
  console.error("Start point 'S' not found");
  process.exit(1);
}

let cur = new Map();
cur.set(sx, 1n);
for (let y = sy; y < h; y++) {
  const next = new Map();
  const row = lines[y];
  for (const [x, cnt] of cur) {
    if (x >= 0 && x < w && row[x] === '^') {
      const add = (k) => {
        const v = next.get(k) ?? 0n;
        next.set(k, v + cnt);
      };
      add(x - 1);
      add(x + 1);
    } else {
      const v = next.get(x) ?? 0n;
      next.set(x, v + cnt);
    }
  }
  cur = next;
}
let total = 0n;
for (const v of cur.values()) total += v;
console.log(total.toString());
