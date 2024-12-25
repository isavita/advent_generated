
type Point = { x: number; y: number };

function solve() {
  const fs = require('fs');
  const grid = fs.readFileSync('input.txt', 'utf-8').split('\n').filter((line: string) => line.length > 0);

  const h = grid.length;
  const w = grid[0].length;
  let S: Point = { x: 0, y: 0 };
  let E: Point = { x: 0, y: 0 };
  const walls: boolean[][] = Array.from({ length: h }, () => Array(w).fill(false));
  const trackCells: Point[] = [];

  for (let i = 0; i < h; i++) {
    for (let j = 0; j < w; j++) {
      const ch = grid[i][j];
      if (ch === 'S') {
        S = { x: i, y: j };
      } else if (ch === 'E') {
        E = { x: i, y: j };
      }
      if (ch === '#') {
        walls[i][j] = true;
      } else {
        trackCells.push({ x: i, y: j });
      }
    }
  }

  const dirs: Point[] = [{ x: 1, y: 0 }, { x: -1, y: 0 }, { x: 0, y: 1 }, { x: 0, y: -1 }];

  function isTrack(x: number, y: number): boolean {
    return x >= 0 && x < h && y >= 0 && y < w && !walls[x][y];
  }

  function normalDistFrom(start: Point): number[][] {
    const dist: number[][] = Array.from({ length: h }, () => Array(w).fill(-1));
    dist[start.x][start.y] = 0;
    const q: Point[] = [start];
    let head = 0;
    while (head < q.length) {
      const cur = q[head++];
      for (const d of dirs) {
        const nx = cur.x + d.x;
        const ny = cur.y + d.y;
        if (nx < 0 || nx >= h || ny < 0 || ny >= w || walls[nx][ny] || dist[nx][ny] >= 0) continue;
        dist[nx][ny] = dist[cur.x][cur.y] + 1;
        q.push({ x: nx, y: ny });
      }
    }
    return dist;
  }

  const distFromS = normalDistFrom(S);
  const distFromE = normalDistFrom(E);

  if (distFromS[E.x][E.y] < 0) {
    console.log(0);
    return;
  }

  const normalCost = distFromS[E.x][E.y];
  const cheats = new Map<string, number>();

  for (const startPos of trackCells) {
    const sd = distFromS[startPos.x][startPos.y];
    if (sd < 0) continue;

    const distC: number[][] = Array.from({ length: h }, () => Array(w).fill(-1));
    distC[startPos.x][startPos.y] = 0;
    const q: Point[] = [startPos];
    let head = 0;

    while (head < q.length) {
      const cur = q[head++];
      const steps = distC[cur.x][cur.y];
      if (steps === 20) continue;
      for (const d of dirs) {
        const nx = cur.x + d.x;
        const ny = cur.y + d.y;
        if (nx < 0 || nx >= h || ny < 0 || ny >= w || distC[nx][ny] >= 0) continue;
        distC[nx][ny] = steps + 1;
        q.push({ x: nx, y: ny });
      }
    }

    for (let x = 0; x < h; x++) {
      for (let y = 0; y < w; y++) {
        const s = distC[x][y];
        if (s > 0 && s <= 20 && isTrack(x, y)) {
          const ed = distFromE[x][y];
          if (ed < 0) continue;
          const cost = sd + s + ed;
          if (cost < normalCost) {
            const key = `${startPos.x},${startPos.y},${x},${y}`;
            const old = cheats.get(key);
            if (!old || cost < old) {
              cheats.set(key, cost);
            }
          }
        }
      }
    }
  }

  let count = 0;
  for (const cost of cheats.values()) {
    const saving = normalCost - cost;
    if (saving >= 100) {
      count++;
    }
  }
  console.log(count);
}

solve();
