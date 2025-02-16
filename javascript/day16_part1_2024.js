
const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const grid = data.trim().split('\n');

const n = grid.length;
const m = grid[0].length;

let sx, sy, ex, ey;
for (let i = 0; i < n; i++) {
  for (let j = 0; j < m; j++) {
    if (grid[i][j] === 'S') {
      [sx, sy] = [i, j];
    } else if (grid[i][j] === 'E') {
      [ex, ey] = [i, j];
    }
  }
}

const dx = [-1, 0, 1, 0];
const dy = [0, 1, 0, -1];

const dist = Array(n).fill(null).map(() =>
  Array(m).fill(null).map(() => [Infinity, Infinity, Infinity, Infinity])
);
dist[sx][sy][1] = 0;

const h = [];
const push = (x, y, d, cost) => {
  h.push({ x, y, d, cost });
  let i = h.length - 1;
  while (i > 0) {
    const p = (i - 1) >> 1;
    if (h[p].cost <= h[i].cost) break;
    [h[p], h[i]] = [h[i], h[p]];
    i = p;
  }
};

const pop = () => {
  const v = h[0];
  h[0] = h[h.length - 1];
  h.pop();
  let i = 0;
  while (true) {
    let l = 2 * i + 1;
    let r = 2 * i + 2;
    let small = i;
    if (l < h.length && h[l].cost < h[small].cost) small = l;
    if (r < h.length && h[r].cost < h[small].cost) small = r;
    if (small === i) break;
    [h[i], h[small]] = [h[small], h[i]];
    i = small;
  }
  return v;
};

push(sx, sy, 1, 0);

while (h.length > 0) {
  const u = pop();
  if (dist[u.x][u.y][u.d] < u.cost) continue;
  if (u.x === ex && u.y === ey) {
    console.log(u.cost);
    process.exit(0);
  }
  for (const ndir of [(u.d + 1) % 4, (u.d + 3) % 4]) {
    const nc = u.cost + 1000;
    if (nc < dist[u.x][u.y][ndir]) {
      dist[u.x][u.y][ndir] = nc;
      push(u.x, u.y, ndir, nc);
    }
  }
  const nx = u.x + dx[u.d];
  const ny = u.y + dy[u.d];
  if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx][ny] !== '#') {
    const nc = u.cost + 1;
    if (nc < dist[nx][ny][u.d]) {
      dist[nx][ny][u.d] = nc;
      push(nx, ny, u.d, nc);
    }
  }
}
