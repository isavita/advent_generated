const fs = require('fs');

const Wall = '#';
const Free = '.';

class P {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }

  neighbours() {
    return [
      new P(this.x, this.y + 1),
      new P(this.x + 1, this.y),
      new P(this.x, this.y - 1),
      new P(this.x - 1, this.y),
    ];
  }
}

class Map {
  constructor(xMax, yMax, grid, AA, ZZ, teleport, portalName, isOuter) {
    this.xMax = xMax;
    this.yMax = yMax;
    this.grid = grid;
    this.AA = AA;
    this.ZZ = ZZ;
    this.teleport = teleport;
    this.portalName = portalName;
    this.isOuter = isOuter;
  }
}

function parse() {
  const grid = {};
  let xMax = 0, yMax = 0;

  const lines = fs.readFileSync('input.txt', 'utf-8').split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    if (line.length > yMax) yMax = line.length;
    for (let j = 0; j < line.length; j++) {
      grid[`${i},${j}`] = line[j];
    }
  }
  xMax = lines.length;

  let aa, zz;
  const isOuter = {};
  const portalName = {};
  const teleport = {};
  const cache = {};

  for (let i = 0; i < xMax; i++) {
    for (let j = 0; j < yMax; j++) {
      const c = grid[`${i},${j}`];
      if (!(c >= 'A' && c <= 'Z')) continue;

      const [pName, pPoint, ok] = extractPortal(grid, new P(i, j));
      if (!ok) continue;

      portalName[`${pPoint.x},${pPoint.y}`] = pName;

      if (pName === 'AA') {
        aa = pPoint;
        isOuter[`${pPoint.x},${pPoint.y}`] = true;
        continue;
      }

      if (pName === 'ZZ') {
        zz = pPoint;
        isOuter[`${pPoint.x},${pPoint.y}`] = true;
        continue;
      }

      if (cache[pName]) {
        const target = cache[pName];
        teleport[`${pPoint.x},${pPoint.y}`] = target;
        teleport[`${target.x},${target.y}`] = pPoint;
      } else {
        cache[pName] = pPoint;
      }

      if (j === 0 || i === 0 || i === xMax - 2 || j === yMax - 2) {
        isOuter[`${pPoint.x},${pPoint.y}`] = true;
      } else {
        isOuter[`${pPoint.x},${pPoint.y}`] = false;
      }
    }
  }

  return new Map(xMax, yMax, grid, aa, zz, teleport, portalName, isOuter);
}

function extractPortal(grid, p) {
  const c1 = grid[`${p.x},${p.y}`];

  const checkPortal = (c2, portalPoint) => {
    if (c2 >= 'A' && c2 <= 'Z') {
      const portalName = c1 + c2;
      if (grid[`${portalPoint.x},${portalPoint.y}`] === '.') {
        return [portalName, portalPoint, true];
      }
    }
    return [null, null, false];
  };

  const c2 = grid[`${p.x + 1},${p.y}`];
  let [portalName, portalPoint, ok] = checkPortal(c2, new P(p.x + 2, p.y));
  if (ok) return [portalName, portalPoint, ok];
  [portalName, portalPoint, ok] = checkPortal(c2, new P(p.x - 1, p.y));
  if (ok) return [portalName, portalPoint, ok];

  const c3 = grid[`${p.x},${p.y + 1}`];
  [portalName, portalPoint, ok] = checkPortal(c3, new P(p.x, p.y + 2));
  if (ok) return [portalName, portalPoint, ok];
  [portalName, portalPoint, ok] = checkPortal(c3, new P(p.x, p.y - 1));
  if (ok) return [portalName, portalPoint, ok];

  return [null, null, false];
}

function BFS(m) {
  const discovered = {};
  const toDo = [];
  discovered[`${m.AA.x},${m.AA.y}`] = true;
  toDo.push(m.AA);

  let depth = 0;

  while (toDo.length > 0) {
    for (let levelSize = toDo.length; levelSize > 0; levelSize--) {
      const curr = toDo.shift();

      if (curr.x === m.ZZ.x && curr.y === m.ZZ.y) return depth;

      for (const n of curr.neighbours()) {
        const dest = m.grid[`${n.x},${n.y}`];

        if (dest === Wall) continue;

        if (dest === Free) {
          if (!discovered[`${n.x},${n.y}`]) {
            discovered[`${n.x},${n.y}`] = true;
            toDo.push(n);
          }
        } else if (dest >= 'A' && dest <= 'Z') {
          const next = m.teleport[`${curr.x},${curr.y}`];
          if (next && !discovered[`${next.x},${next.y}`]) {
            discovered[`${next.x},${next.y}`] = true;
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
console.log(BFS(m));