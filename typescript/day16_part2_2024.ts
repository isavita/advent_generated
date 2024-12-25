
import * as fs from 'fs';

interface State {
  x: number;
  y: number;
  d: number;
}

interface Node {
  x: number;
  y: number;
  d: number;
  cost: number;
}

class MinHeap {
  a: Node[] = [];

  push(v: Node) {
    this.a.push(v);
    this.up(this.a.length - 1);
  }

  pop(): Node {
    const v = this.a[0];
    this.a[0] = this.a[this.a.length - 1];
    this.a.pop();
    this.down(0);
    return v;
  }

  up(i: number) {
    while (i > 0) {
      const p = (i - 1) >> 1;
      if (this.a[p].cost <= this.a[i].cost) {
        break;
      }
      [this.a[p], this.a[i]] = [this.a[i], this.a[p]];
      i = p;
    }
  }

  down(i: number) {
    while (true) {
      const l = 2 * i + 1;
      const r = 2 * i + 2;
      let small = i;
      if (l < this.a.length && this.a[l].cost < this.a[small].cost) {
        small = l;
      }
      if (r < this.a.length && this.a[r].cost < this.a[small].cost) {
        small = r;
      }
      if (small === i) {
        break;
      }
      [this.a[i], this.a[small]] = [this.a[small], this.a[i]];
      i = small;
    }
  }
}

const input = fs.readFileSync('input.txt', 'utf-8');
const grid = input.split('\n').filter((line) => line.length > 0);

const n = grid.length;
const m = grid[0].length;
let sx = 0,
  sy = 0,
  ex = 0,
  ey = 0;
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

const dx = [-1, 0, 1, 0];
const dy = [0, 1, 0, -1];

const dist: number[][][] = Array.from({ length: n }, () =>
  Array.from({ length: m }, () => Array(4).fill(Infinity))
);
dist[sx][sy][1] = 0;

const h = new MinHeap();
h.push({ x: sx, y: sy, d: 1, cost: 0 });

while (h.a.length > 0) {
  const u = h.pop();
  if (dist[u.x][u.y][u.d] < u.cost) {
    continue;
  }
  if (u.x === ex && u.y === ey) {
    continue;
  }
  for (const ndir of [(u.d + 1) % 4, (u.d + 3) % 4]) {
    const nc = u.cost + 1000;
    if (nc < dist[u.x][u.y][ndir]) {
      dist[u.x][u.y][ndir] = nc;
      h.push({ x: u.x, y: u.y, d: ndir, cost: nc });
    }
  }
  const nx = u.x + dx[u.d];
  const ny = u.y + dy[u.d];
  if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx][ny] !== '#') {
    const nc = u.cost + 1;
    if (nc < dist[nx][ny][u.d]) {
      dist[nx][ny][u.d] = nc;
      h.push({ x: nx, y: ny, d: u.d, cost: nc });
    }
  }
}

let best = Infinity;
for (let d = 0; d < 4; d++) {
  best = Math.min(best, dist[ex][ey][d]);
}

const used: boolean[][] = Array.from({ length: n }, () => Array(m).fill(false));

let rev: State[] = [];
for (let d = 0; d < 4; d++) {
  if (dist[ex][ey][d] === best) {
    rev.push({ x: ex, y: ey, d });
  }
}

const vis: boolean[][][] = Array.from({ length: n }, () =>
  Array.from({ length: m }, () => Array(4).fill(false))
);
for (const s of rev) {
  vis[s.x][s.y][s.d] = true;
}

while (rev.length > 0) {
  const u = rev.pop()!;
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
