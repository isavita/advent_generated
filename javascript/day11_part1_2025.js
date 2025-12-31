
const fs = require('fs');

function countPaths(cur, target, adj, memo) {
  if (cur === target) return 1n;
  if (memo.has(cur)) return memo.get(cur);
  const neighbors = adj.get(cur) || [];
  let total = 0n;
  for (const nxt of neighbors) total += countPaths(nxt, target, adj, memo);
  memo.set(cur, total);
  return total;
}

function main() {
  const lines = fs.readFileSync('input.txt', 'utf8').split(/\r?\n/);
  const adj = new Map();
  for (let line of lines) {
    line = line.trim();
    if (!line) continue;
    const parts = line.split(':');
    if (parts.length === 2) {
      const src = parts[0].trim();
      const dests = parts[1].trim().split(/\s+/).filter(Boolean);
      adj.set(src, dests);
    }
  }
  const result = countPaths('you', 'out', adj, new Map());
  console.log(result.toString());
}

main();
