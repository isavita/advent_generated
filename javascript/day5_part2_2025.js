
const fs = require('fs');

function main() {
  const lines = fs.readFileSync('input.txt', 'utf8')
    .split(/\r?\n/)
    .map(l => l.trim())
    .filter(l => l);

  const ranges = [];
  for (const line of lines) {
    const parts = line.split('-');
    if (parts.length !== 2) continue;
    try {
      let a = BigInt(parts[0]);
      let b = BigInt(parts[1]);
      if (a > b) [a, b] = [b, a];
      ranges.push([a, b]);
    } catch {}
  }

  if (!ranges.length) {
    console.log('Total fresh IDs: 0');
    return;
  }

  ranges.sort((p, q) => (p[0] < q[0] ? -1 : p[0] > q[0] ? 1 : 0));

  let total = 0n;
  let [curMin, curMax] = ranges[0];

  for (let i = 1; i < ranges.length; i++) {
    const [min, max] = ranges[i];
    if (min <= curMax) {
      if (max > curMax) curMax = max;
    } else {
      total += curMax - curMin + 1n;
      curMin = min;
      curMax = max;
    }
  }
  total += curMax - curMin + 1n;

  console.log(`Total fresh IDs: ${total}`);
}

main();
