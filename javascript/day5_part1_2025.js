
const fs = require('fs');

function main() {
  const lines = fs.readFileSync('input.txt', 'utf8').split(/\r?\n/);
  let parsing = true;
  const ranges = [];
  const ids = [];

  for (const line of lines) {
    const s = line.trim();
    if (!s) {
      if (parsing) parsing = false;
      continue;
    }
    if (parsing) {
      const [a, b] = s.split('-');
      ranges.push([BigInt(a), BigInt(b)]);
    } else {
      ids.push(BigInt(s));
    }
  }

  ranges.sort((x, y) => (x[0] < y[0] ? -1 : x[0] > y[0] ? 1 : 0));

  const merged = [];
  for (const [mn, mx] of ranges) {
    if (!merged.length || mn > merged[merged.length - 1][1]) merged.push([mn, mx]);
    else if (mx > merged[merged.length - 1][1]) merged[merged.length - 1][1] = mx;
  }

  function contains(v) {
    let l = 0,
      r = merged.length;
    while (l < r) {
      const m = (l + r) >> 1;
      const [lo, hi] = merged[m];
      if (v < lo) r = m;
      else if (v > hi) l = m + 1;
      else return true;
    }
    return false;
  }

  let fresh = 0;
  for (const id of ids) if (contains(id)) ++fresh;

  console.log(`Number of fresh ingredients: ${fresh}`);
}

main();
