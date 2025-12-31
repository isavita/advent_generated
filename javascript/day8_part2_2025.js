
const fs = require('fs');

function distSq(a, b) {
  const dx = a[0] - b[0];
  const dy = a[1] - b[1];
  const dz = a[2] - b[2];
  return dx * dx + dy * dy + dz * dz;
}

function find(p, x) {
  while (p[x] !== x) {
    p[x] = p[p[x]];
    x = p[x];
  }
  return x;
}

function union(p, r, a, b) {
  if (r[a] < r[b]) p[a] = b;
  else if (r[a] > r[b]) p[b] = a;
  else { p[b] = a; r[a]++; }
}

function main() {
  const lines = fs.readFileSync('input.txt', 'utf8')
    .split('\n')
    .map(l => l.trim())
    .filter(l => l);

  const pts = [];
  for (const line of lines) {
    const parts = line.split(',');
    if (parts.length !== 3) continue;
    const x = Number(parts[0]), y = Number(parts[1]), z = Number(parts[2]);
    if (Number.isNaN(x) || Number.isNaN(y) || Number.isNaN(z)) continue;
    pts.push([x, y, z]);
  }

  if (pts.length < 2) return;

  const n = pts.length;
  const edges = [];
  for (let i = 0; i < n; i++) {
    for (let j = i + 1; j < n; j++) {
      edges.push([i, j, distSq(pts[i], pts[j])]);
    }
  }

  edges.sort((a, b) => a[2] - b[2]);

  const parent = Array.from({ length: n }, (_, i) => i);
  const rank = new Array(n).fill(0);
  let comps = n;

  for (const [u, v] of edges) {
    if (comps === 1) break;
    const ru = find(parent, u);
    const rv = find(parent, v);
    if (ru !== rv) {
      union(parent, rank, ru, rv);
      comps--;
      if (comps === 1) {
        const p1 = pts[u], p2 = pts[v];
        console.log(`Connected ${p1[0]},${p1[1]},${p1[2]} and ${p2[0]},${p2[1]},${p2[2]}`);
        console.log(`Product of X coordinates: ${p1[0] * p2[0]}`);
      }
    }
  }
}

main();
