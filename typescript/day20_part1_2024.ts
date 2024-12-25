
import * as fs from 'fs';

type Point = { x: number; y: number };

function solve() {
  const grid = fs.readFileSync('input.txt', 'utf-8').split('\n').filter(line => line.trim() !== '');
  const h = grid.length;
  const w = grid[0].length;

  let S: Point = { x: 0, y: 0 };
  let E: Point = { x: 0, y: 0 };
  const trackCells: Point[] = [];
  const walls: boolean[][] = Array.from({ length: h }, () => Array(w).fill(false));

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

  const normalDistFrom = (start: Point): number[][] => {
    const dist: number[][] = Array.from({ length: h }, () => Array(w).fill(-1));
    dist[start.x][start.y] = 0;
    const q: Point[] = [start];
    let head = 0;
    while (head < q.length) {
      const cur = q[head++];
      for (const d of dirs) {
        const nx = cur.x + d.x;
        const ny = cur.y + d.y;
        if (nx < 0 || nx >= h || ny < 0 || ny >= w || walls[nx][ny] || dist[nx][ny] !== -1) {
          continue;
        }
        dist[nx][ny] = dist[cur.x][cur.y] + 1;
        q.push({ x: nx, y: ny });
      }
    }
    return dist;
  };

  const distFromS = normalDistFrom(S);
  const distFromE = normalDistFrom(E);

  if (distFromS[E.x][E.y] === -1) {
    console.log(0);
    return;
  }

  const normalCost = distFromS[E.x][E.y];

  const isTrack = (x: number, y: number): boolean => {
    return x >= 0 && x < h && y >= 0 && y < w && !walls[x][y];
  };

  let possibleCheats = 0;
  const cheatDist: number[][][] = Array.from({ length: h }, () =>
    Array.from({ length: w }, () => Array(3).fill(Infinity))
  );

  for (const startPos of trackCells) {
    const sd = distFromS[startPos.x][startPos.y];
    if (sd === -1) continue;

    const q: { p: Point; steps: number }[] = [{ p: startPos, steps: 0 }];
    let head = 0;
    cheatDist[startPos.x][startPos.y][0] = 0;

    while (head < q.length) {
      const { p, steps } = q[head++];

      if (steps === 2) continue;

      for (const d of dirs) {
        const np = { x: p.x + d.x, y: p.y + d.y };
        if (np.x < 0 || np.x >= h || np.y < 0 || np.y >= w) continue;

        if (cheatDist[np.x][np.y][steps + 1] > cheatDist[p.x][p.y][steps] + 1) {
          cheatDist[np.x][np.y][steps + 1] = cheatDist[p.x][p.y][steps] + 1;
          q.push({ p: np, steps: steps + 1 });
        }
      }
    }

    for (let i = 0; i < h; i++) {
      for (let j = 0; j < w; j++) {
        if (isTrack(i, j) && cheatDist[i][j][2] <= 2) {
          const ed = distFromE[i][j];
          if (ed !== -1) {
            const newCost = sd + 2 + ed;
            if (normalCost - newCost >= 100) {
              possibleCheats++;
            }
          }
        }
      }
    }
    for (let i = 0; i < h; i++) {
      for (let j = 0; j < w; j++) {
        cheatDist[i][j].fill(Infinity);
      }
    }
  }

  console.log(possibleCheats);
}

solve();
