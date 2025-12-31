
const fs = require('fs');

function main() {
  const data = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
  const points = [];
  for (let line of data) {
    line = line.trim();
    if (!line) continue;
    const [x, y, z] = line.split(',').map(Number);
    points.push({ x, y, z });
  }
  const n = points.length;
  if (n < 2) {
    console.log('Not enough points to form circuits.');
    return;
  }

  const edges = [];
  for (let i = 0; i < n; i++) {
    const p1 = points[i];
    for (let j = i + 1; j < n; j++) {
      const p2 = points[j];
      const dx = p1.x - p2.x;
      const dy = p1.y - p2.y;
      const dz = p1.z - p2.z;
      const d = dx * dx + dy * dy + dz * dz;
      edges.push({ u: i, v: j, d });
    }
  }

  edges.sort((a, b) => a.d - b.d);

  const parent = new Uint32Array(n);
  const size = new Uint32Array(n);
  for (let i = 0; i < n; i++) {
    parent[i] = i;
    size[i] = 1;
  }

  function find(i) {
    while (parent[i] !== i) {
      parent[i] = parent[parent[i]];
      i = parent[i];
    }
    return i;
  }

  function union(i, j) {
    let ri = find(i);
    let rj = find(j);
    if (ri === rj) return;
    if (size[ri] < size[rj]) {
      const t = ri;
      ri = rj;
      rj = t;
    }
    parent[rj] = ri;
    size[ri] += size[rj];
  }

  const limit = Math.min(1000, edges.length);
  for (let k = 0; k < limit; k++) {
    const e = edges[k];
    union(e.u, e.v);
  }

  const compSizes = [];
  for (let i = 0; i < n; i++) {
    if (parent[i] === i) compSizes.push(size[i]);
  }

  compSizes.sort((a, b) => b - a);
  const top3 = compSizes.slice(0, 3);
  const result = top3.reduce((acc, v) => acc * BigInt(v), 1n);
  console.log(`Product of three largest circuit sizes: ${result}`);
}

main();
