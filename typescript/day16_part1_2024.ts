
const fs = require('fs');

const grid: string[] = fs.readFileSync('input.txt', 'utf-8').split('\n');
const n: number = grid.length;
const m: number = grid[0].length;
let sx: number = 0, sy: number = 0, ex: number = 0, ey: number = 0;

for (let i = 0; i < n; i++) {
  for (let j = 0; j < m; j++) {
    if (grid[i][j] === 'S') {
      sx = i;
      sy = j;
    } else if (grid[i][j] === 'E') {
      ex = i;
      ey = j;
    }
  }
}

const dx: number[] = [-1, 0, 1, 0];
const dy: number[] = [0, 1, 0, -1];
const dist: number[][][] = Array.from({ length: n }, () =>
  Array.from({ length: m }, () => Array(4).fill(Infinity))
);
dist[sx][sy][1] = 0;

const h: { x: number; y: number; d: number; cost: number }[] = [];
const push = (v: { x: number; y: number; d: number; cost: number }) => {
  h.push(v);
  let i = h.length - 1;
  while (i > 0) {
    const p = (i - 1) >> 1;
    if (h[p].cost <= h[i].cost) break;
    [h[p], h[i]] = [h[i], h[p]];
    i = p;
  }
};

const pop = (): { x: number; y: number; d: number; cost: number } => {
  const v = h[0];
  h[0] = h[h.length - 1];
  h.pop();
  let i = 0;
  while (true) {
    const l = 2 * i + 1;
    const r = 2 * i + 2;
    let small = i;
    if (l < h.length && h[l].cost < h[small].cost) small = l;
    if (r < h.length && h[r].cost < h[small].cost) small = r;
    if (small === i) break;
    [h[i], h[small]] = [h[small], h[i]];
    i = small;
  }
  return v;
};

push({ x: sx, y: sy, d: 1, cost: 0 });

while (h.length > 0) {
  const u = pop();
  if (dist[u.x][u.y][u.d] < u.cost) continue;
  if (u.x === ex && u.y === ey) {
    console.log(u.cost);
    break;
  }
  for (const ndir of [(u.d + 1) % 4, (u.d + 3) % 4]) {
    const nc = u.cost + 1000;
    if (nc < dist[u.x][u.y][ndir]) {
      dist[u.x][u.y][ndir] = nc;
      push({ x: u.x, y: u.y, d: ndir, cost: nc });
    }
  }
  const nx = u.x + dx[u.d];
  const ny = u.y + dy[u.d];
  if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx][ny] !== '#') {
    const nc = u.cost + 1;
    if (nc < dist[nx][ny][u.d]) {
      dist[nx][ny][u.d] = nc;
      push({ x: nx, y: ny, d: u.d, cost: nc });
    }
  }
}
