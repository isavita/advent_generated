
import * as fs from 'fs';

const data = fs.readFileSync('input.txt', 'utf-8');
const lines = data.trim().split('\n');
const nr = lines.length;
const nc = lines[0].length;
const grid = lines.map(line => line.split('').map(Number));

const dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]];
const trailheads: [number, number][] = [];
for (let r = 0; r < nr; r++) {
  for (let c = 0; c < nc; c++) {
    if (grid[r][c] === 0) {
      trailheads.push([r, c]);
    }
  }
}

let sumScores = 0;
for (const th of trailheads) {
  const reached = new Set<string>();
  const front: { p: [number, number]; h: number }[] = [{ p: th, h: 0 }];
  const visited = new Set<string>();
  while (front.length > 0) {
    const cur = front.pop()!;
    if (cur.h === 9) {
      const key = `${cur.p[0]},${cur.p[1]}`;
      if (!reached.has(key)) {
        reached.add(key);
      }
      continue;
    }
    for (const d of dirs) {
      const nr2 = cur.p[0] + d[0];
      const nc2 = cur.p[1] + d[1];
      if (nr2 < 0 || nr2 >= nr || nc2 < 0 || nc2 >= nc) continue;
      if (grid[nr2][nc2] === cur.h + 1) {
        const key = `${nr2},${nc2},${cur.h + 1}`;
        if (!visited.has(key)) {
          visited.add(key);
          front.push({ p: [nr2, nc2], h: cur.h + 1 });
        }
      }
    }
  }
  sumScores += reached.size;
}

console.log(sumScores);
