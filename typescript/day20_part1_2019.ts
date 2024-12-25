
type char = string;
type P = { x: number; y: number };

function neighbours(p: P): P[] {
  return [
    { x: p.x, y: p.y + 1 },
    { x: p.x + 1, y: p.y },
    { x: p.x, y: p.y - 1 },
    { x: p.x - 1, y: p.y },
  ];
}

type Map = {
  xMax: number;
  yMax: number;
  grid: Record<string, char>;
  aa: P;
  zz: P;
  teleport: Record<string, P>;
  portalName: Record<string, string>;
  isOuter: Record<string, boolean>;
};

function parse(): Map {
  const fs = require('fs');
  const lines = fs.readFileSync('input.txt', 'utf-8').split('\n');
  const grid: Record<string, char> = {};
  let xMax = 0;
  let yMax = 0;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    yMax = Math.max(yMax, line.length);
    for (let j = 0; j < line.length; j++) {
      grid[key(i, j)] = line[j];
    }
    xMax = i + 1;
  }

  let aa: P = { x: 0, y: 0 };
  let zz: P = { x: 0, y: 0 };
  const isOuter: Record<string, boolean> = {};
  const portalName: Record<string, string> = {};
  const teleport: Record<string, P> = {};
  const cache: Record<string, P> = {};

  for (let i = 0; i < xMax; i++) {
    for (let j = 0; j < yMax; j++) {
      const c = grid[key(i, j)];
      if (!isLetter(c)) continue;

      const [pName, pPoint, ok] = extractPortal(grid, { x: i, y: j });
      if (!ok) continue;

      portalName[keyP(pPoint)] = pName;

      if (pName === 'AA') {
        aa = pPoint;
        isOuter[keyP(pPoint)] = true;
        continue;
      }

      if (pName === 'ZZ') {
        zz = pPoint;
        isOuter[keyP(pPoint)] = true;
        continue;
      }

      if (cache[pName]) {
        teleport[keyP(pPoint)] = cache[pName];
        teleport[keyP(cache[pName])] = pPoint;
      } else {
        cache[pName] = pPoint;
      }

      if (j === 0 || i === 0 || i === xMax - 2 || j === yMax - 2) {
        isOuter[keyP(pPoint)] = true;
      } else {
        isOuter[keyP(pPoint)] = false;
      }
    }
  }

  return { xMax, yMax, grid, aa, zz, teleport, portalName, isOuter };
}

function extractPortal(grid: Record<string, char>, p: P): [string, P, boolean] {
  const c1 = grid[keyP(p)];

  if (isLetter(grid[key(p.x + 1, p.y)])) {
    const portalName = c1 + grid[key(p.x + 1, p.y)];
    let portalPoint = { x: p.x + 2, y: p.y };
    if (grid[keyP(portalPoint)] === '.') return [portalName, portalPoint, true];
    portalPoint = { x: p.x - 1, y: p.y };
    if (grid[keyP(portalPoint)] === '.') return [portalName, portalPoint, true];
  }

  if (isLetter(grid[key(p.x, p.y + 1)])) {
    const portalName = c1 + grid[key(p.x, p.y + 1)];
    let portalPoint = { x: p.x, y: p.y + 2 };
    if (grid[keyP(portalPoint)] === '.') return [portalName, portalPoint, true];
    portalPoint = { x: p.x, y: p.y - 1 };
    if (grid[keyP(portalPoint)] === '.') return [portalName, portalPoint, true];
  }

  return ['', { x: 0, y: 0 }, false];
}

function key(x: number, y: number): string {
  return `${x},${y}`;
}

function keyP(p: P): string {
  return `${p.x},${p.y}`;
}

function isLetter(c: char): boolean {
  return c >= 'A' && c <= 'Z';
}

function bfs(m: Map): number {
  const discovered: Record<string, boolean> = {};
  let toDo: P[] = [];
  discovered[keyP(m.aa)] = true;
  toDo.push(m.aa);
  let depth = 0;

  while (toDo.length > 0) {
    const levelSize = toDo.length;
    for (let i = 0; i < levelSize; i++) {
      const curr = toDo.shift()!;
      if (curr.x === m.zz.x && curr.y === m.zz.y) return depth;

      for (const n of neighbours(curr)) {
        const dest = m.grid[keyP(n)];
        if (dest === '#') continue;

        if (dest === '.') {
          if (!discovered[keyP(n)]) {
            discovered[keyP(n)] = true;
            toDo.push(n);
          }
        } else if (isLetter(dest)) {
          const next = m.teleport[keyP(curr)];
          if (next && !discovered[keyP(next)]) {
            discovered[keyP(next)] = true;
            toDo.push(next);
          }
        }
      }
    }
    depth++;
  }

  return -1;
}

const m = parse();
console.log(bfs(m));
