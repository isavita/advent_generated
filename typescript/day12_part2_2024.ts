
import * as fs from 'fs';

interface Move {
  label: string;
  x: number;
  y: number;
}

const input = fs.readFileSync('input.txt', 'utf-8');
const args = input.split('\n');
const graph: string[][] = [];

for (const line of args) {
  if (line === '') {
    continue;
  }
  graph.push(line.split(''));
}

const H = graph.length;
const W = graph[0].length;

const move: Move[] = [
  { label: 'left', x: -1, y: 0 },
  { label: 'up', x: 0, y: -1 },
  { label: 'right', x: 1, y: 0 },
  { label: 'down', x: 0, y: 1 },
];

let sum = 0;

for (let y = 0; y < H; y++) {
  for (let x = 0; x < W; x++) {
    if (graph[y][x] === '.') {
      continue;
    }

    let area = 0;
    const target = graph[y][x];
    const visited = new Set<string>();
    const side = new Map<string, Set<string>>();

    const search = (cx: number, cy: number, label: string) => {
      if (graph[cy][cx] !== target) {
        if (label !== '' && !visited.has(`${cx},${cy}`)) {
          saveOuter(label, side, cx, cy);
        }
        return;
      }

      visited.add(`${cx},${cy}`);
      area++;
      graph[cy][cx] = '.';

      for (const m of move) {
        const nx = cx + m.x;
        const ny = cy + m.y;

        if (nx < 0 || nx >= W || ny < 0 || ny >= H) {
          saveOuter(m.label, side, nx, ny);
          continue;
        }
        search(nx, ny, m.label);
      }
    };

    search(x, y, '');
    const outer = countOuter(side);
    sum += area * outer;
  }
}
console.log(sum);

function saveOuter(label: string, side: Map<string, Set<string>>, x: number, y: number) {
  let key: string;
  if (label === 'up' || label === 'down') {
    key = `${y}:${x}`;
  } else {
    key = `${x}:${y}`;
  }

  if (!side.has(label)) {
    side.set(label, new Set<string>());
  }
  side.get(label)!.add(key);
}

function countOuter(side: Map<string, Set<string>>) {
  let outer = 0;
  for (const label of side.keys()) {
    const array = Array.from(side.get(label)!).sort((a, b) => {
      const [aFirst, aSecond] = a.split(':').map(Number);
      const [bFirst, bSecond] = b.split(':').map(Number);
      if (aFirst === bFirst) {
        return aSecond - bSecond;
      }
      return aFirst - bFirst;
    });

    const temp: string[] = [];
    for (const current of array) {
      const [i, j] = current.split(':').map(Number);
      if (!check(temp, i, j)) {
        outer++;
      }
      temp.push(current);
    }
  }
  return outer;
}

function check(ary: string[], i: number, j: number) {
  const search = [`${i}:${j - 1}`, `${i}:${j + 1}`];
  for (const s of search) {
    if (ary.includes(s)) {
      return true;
    }
  }
  return false;
}
