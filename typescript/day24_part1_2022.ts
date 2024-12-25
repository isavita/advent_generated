
type Point = { x: number; y: number };

type State = {
  pos: Point;
  step: number;
};

const readFileSync = require('fs').readFileSync;
const input = readFileSync('input.txt', 'utf-8').trim();
const lines = input.split('\n');

const grid: Map<string, string> = new Map();
for (let y = 0; y < lines.length; y++) {
  for (let x = 0; x < lines[y].length; x++) {
    if (lines[y][x] !== '.') {
      grid.set(`${x},${y}`, lines[y][x]);
    }
  }
}

const bounds = getBounds(Array.from(grid.keys()));
const entrance: Point = { x: 1, y: 0 };
const exit: Point = { x: bounds.maxX - 2, y: bounds.maxY - 1 };

const crossing = steps(grid, bounds, entrance, exit, 0);
console.log(crossing);

function steps(
  grid: Map<string, string>,
  bounds: { maxX: number; maxY: number },
  start: Point,
  end: Point,
  initialStep: number
): number {
  const q: State[] = [{ pos: start, step: initialStep }];
  const seen: Set<string> = new Set();

  const neighbors4: Point[] = [
    { x: 0, y: 1 },
    { x: 0, y: -1 },
    { x: 1, y: 0 },
    { x: -1, y: 0 },
  ];

  while (q.length > 0) {
    const curr = q.shift()!;
    if (curr.pos.x === end.x && curr.pos.y === end.y) {
      return curr.step;
    }

    for (const n of [...neighbors4, { x: 0, y: 0 }]) {
      const nextPos: Point = {
        x: curr.pos.x + n.x,
        y: curr.pos.y + n.y,
      };
      const nextState: State = { pos: nextPos, step: curr.step + 1 };
      const nextStateKey = `${nextState.pos.x},${nextState.pos.y},${nextState.step}`;

      if (seen.has(nextStateKey)) continue;
      if (
        nextState.pos.x < 0 ||
        nextState.pos.y < 0 ||
        nextState.pos.x >= bounds.maxX ||
        nextState.pos.y >= bounds.maxY
      )
        continue;
      if (grid.get(`${nextState.pos.x},${nextState.pos.y}`) === '#') continue;

      if (
        nextState.pos.y > 0 &&
        nextState.pos.y < bounds.maxY - 1
      ) {
        let blocked = false;
        for (const bliz of ['^', '>', 'v', '<']) {
          const prev = getPrevPos(nextState.pos, bliz, nextState.step, bounds);
          if (grid.get(`${prev.x},${prev.y}`) === bliz) {
            blocked = true;
            break;
          }
        }
        if (blocked) continue;
      }

      q.push(nextState);
      seen.add(nextStateKey);
    }
  }
  return -1;
}

function getBounds(keys: string[]): { maxX: number; maxY: number } {
  let maxX = 0;
  let maxY = 0;
  for (const key of keys) {
    const [x, y] = key.split(',').map(Number);
    maxX = Math.max(maxX, x + 1);
    maxY = Math.max(maxY, y + 1);
  }
  return { maxX, maxY };
}

function getPrevPos(
  pos: Point,
  bliz: string,
  step: number,
  bounds: { maxX: number; maxY: number }
): Point {
  const dir = dirFromByte(bliz);
  const dx = dir.x * step;
  const dy = dir.y * step;
  let prevX = pos.x - dx;
  let prevY = pos.y - dy;

  prevX = ((prevX - 1) % (bounds.maxX - 2)) + 1;
  if (prevX <= 0) prevX += bounds.maxX - 2;

  prevY = ((prevY - 1) % (bounds.maxY - 2)) + 1;
  if (prevY <= 0) prevY += bounds.maxY - 2;

  return { x: prevX, y: prevY };
}

function dirFromByte(b: string): Point {
  const pointReversed: { [key: string]: Point } = {
    '^': { x: 0, y: -1 },
    '>': { x: 1, y: 0 },
    v: { x: 0, y: 1 },
    '<': { x: -1, y: 0 },
  };
  return pointReversed[b];
}
