
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const grid = input;
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

const dist = Array(n).fill(null).map(() => Array(m).fill(null).map(() => Array(4).fill(Infinity)));
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
    if (h.length === 0) return undefined;

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
  if (u.x === ex && u.y === ey) continue;

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

let best = Infinity;
for (let d = 0; d < 4; d++) {
  best = Math.min(best, dist[ex][ey][d]);
}

const used = Array(n).fill(null).map(() => Array(m).fill(false));
let rev = [];
for (let d = 0; d < 4; d++) {
  if (dist[ex][ey][d] === best) {
    rev.push({ x: ex, y: ey, d });
  }
}

const vis = Array(n).fill(null).map(() => Array(m).fill(null).map(() => Array(4).fill(false)));
for (const s of rev) {
  vis[s.x][s.y][s.d] = true;
}

while (rev.length > 0) {
  const u = rev.pop();
  used[u.x][u.y] = true;
  const costU = dist[u.x][u.y][u.d];

  for (const pd of [(u.d + 1) % 4, (u.d + 3) % 4]) {
    if (dist[u.x][u.y][pd] === costU - 1000) {
      if (!vis[u.x][u.y][pd]) {
        vis[u.x][u.y][pd] = true;
        rev.push({ x: u.x, y: u.y, d: pd });
      }
    }
  }

  const px = u.x - dx[u.d];
  const py = u.y - dy[u.d];
  if (px >= 0 && px < n && py >= 0 && py < m && grid[px][py] !== '#') {
    if (dist[px][py][u.d] === costU - 1) {
      if (!vis[px][py][u.d]) {
        vis[px][py][u.d] = true;
        rev.push({ x: px, y: py, d: u.d });
      }
    }
  }
}

let cnt = 0;
for (let i = 0; i < n; i++) {
  for (let j = 0; j < m; j++) {
    if (used[i][j] && grid[i][j] !== '#') {
      cnt++;
    }
  }
}

console.log(cnt);
