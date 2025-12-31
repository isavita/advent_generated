
// ---------------  main.js  ----------------
const fs = require('fs');

// read adjacency list
const adj = new Map();
fs.readFileSync('input.txt', 'utf8')
  .split('\n')
  .forEach(line => {
    const l = line.trim();
    if (!l) return;
    const parts = l.split(':');
    if (parts.length !== 2) return;
    const src = parts[0].trim();
    const dests = parts[1].trim().split(/\s+/).filter(Boolean);
    adj.set(src, dests);
  });

// count paths with memoisation (returns BigInt)
function countPaths(start, end) {
  const memo = new Map();
  const dfs = cur => {
    if (cur === end) return 1n;
    if (memo.has(cur)) return memo.get(cur);
    let total = 0n;
    for (const nxt of adj.get(cur) || []) total += dfs(nxt);
    memo.set(cur, total);
    return total;
  };
  return dfs(start);
}

// compute required products
const s1 = countPaths('svr', 'dac') *
           countPaths('dac', 'fft') *
           countPaths('fft', 'out');

const s2 = countPaths('svr', 'fft') *
           countPaths('fft', 'dac') *
           countPaths('dac', 'out');

console.log(`Paths (svr->dac->fft->out): ${s1}`);
console.log(`Paths (svr->fft->dac->out): ${s2}`);
console.log(`Total paths visiting both: ${s1 + s2}`);
