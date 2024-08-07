import * as fs from 'fs';

interface Point {
  x: number;
  y: number;
}

interface DoorMap {
  [key: string]: { [key: string]: boolean };
}

const regex = fs.readFileSync('input.txt', 'utf8');
const dm = buildMap(regex.slice(1, -1));
const maxDoors = findFurthestRoom(dm);
console.log(maxDoors);

function buildMap(regex: string): DoorMap {
  const dm: DoorMap = {};
  const stack: Point[] = [];
  let cp: Point = { x: 0, y: 0 };
  for (const c of regex) {
    if (c === '(') {
      stack.push(cp);
    } else if (c === '|') {
      cp = stack[stack.length - 1];
    } else if (c === ')') {
      cp = stack[stack.length - 1];
      stack.pop();
    } else {
      const np = move(cp, c);
      if (!dm[`${cp.x},${cp.y}`]) {
        dm[`${cp.x},${cp.y}`] = {};
      }
      dm[`${cp.x},${cp.y}`][`${np.x},${np.y}`] = true;
      cp = np;
    }
  }
  return dm;
}

function move(p: Point, dir: string): Point {
  switch (dir) {
    case 'N':
      return { x: p.x, y: p.y - 1 };
    case 'S':
      return { x: p.x, y: p.y + 1 };
    case 'E':
      return { x: p.x + 1, y: p.y };
    case 'W':
      return { x: p.x - 1, y: p.y };
    default:
      return p;
  }
}

function findFurthestRoom(dm: DoorMap): number {
  const visited: { [key: string]: number } = {};
  const queue: Point[] = [{ x: 0, y: 0 }];
  let maxDoors = 0;

  while (queue.length > 0) {
    const p = queue.shift() as Point;
    for (const np in dm[`${p.x},${p.y}`]) {
      if (!visited[np]) {
        visited[np] = (visited[`${p.x},${p.y}`] || 0) + 1;
        maxDoors = Math.max(maxDoors, visited[np]);
        queue.push({ x: parseInt(np.split(',')[0]), y: parseInt(np.split(',')[1]) });
      }
    }
  }
  return maxDoors;
}