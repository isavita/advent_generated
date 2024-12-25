
const fs = require('fs');

type P = { x: number; y: number };

class Elf {
  pos: P;
  moving: boolean = false;
  nextPos: P = { x: 0, y: 0 };

  constructor(pos: P) {
    this.pos = pos;
  }
}

const N = 1;
const E = 3;
const S = 5;
const W = 7;

const dirs: P[] = [
  { x: -1, y: -1 },
  { x: -1, y: 0 },
  { x: -1, y: 1 },
  { x: 0, y: 1 },
  { x: 1, y: 1 },
  { x: 1, y: 0 },
  { x: 1, y: -1 },
  { x: 0, y: -1 },
];

let map = new Map<string, boolean>();
let elves: Elf[] = [];
let order = [N, S, W, E];
let currDir = 0;

function key(p: P): string {
  return `${p.x},${p.y}`;
}

function aroundAllEmpty(e: Elf): boolean {
  for (const d of dirs) {
    const adj: P = { x: e.pos.x + d.x, y: e.pos.y + d.y };
    if (map.has(key(adj))) {
      return false;
    }
  }
  return true;
}

function elfInDirection(e: Elf, wannaGo: number): boolean {
  for (let j = -1; j <= 1; j++) {
    const dxy = dirs[(wannaGo + j + 8) % 8];
    const adj: P = { x: e.pos.x + dxy.x, y: e.pos.y + dxy.y };
    if (map.has(key(adj))) {
      return true;
    }
  }
  return false;
}

function run(): boolean {
  const proposes = new Map<string, number>();
  const elvesPositions = new Set<string>();

  for (const e of elves) {
    elvesPositions.add(key(e.pos));
  }

  for (const e of elves) {
    if (aroundAllEmpty(e)) {
      continue;
    }

    for (let i = 0; i < 4; i++) {
      const dir = order[(currDir + i) % 4];

      if (elfInDirection(e, dir)) {
        continue;
      }

      const dxy = dirs[dir];
      const dest: P = { x: e.pos.x + dxy.x, y: e.pos.y + dxy.y };
      const destKey = key(dest);
      proposes.set(destKey, (proposes.get(destKey) || 0) + 1);
      e.nextPos = dest;
      e.moving = true;
      break;
    }
  }

  let someoneMoved = false;
  for (const e of elves) {
    if (!e.moving) {
      continue;
    }

    if ((proposes.get(key(e.nextPos)) || 0) > 1) {
      e.moving = false;
      continue;
    }

    someoneMoved = true;
    map.delete(key(e.pos));
    map.set(key(e.nextPos), true);
    e.pos = e.nextPos;
    e.moving = false;
  }

  currDir = (currDir + 1) % 4;

  return someoneMoved;
}

function parse() {
  const fileContent = fs.readFileSync('input.txt', 'utf-8');
  const lines = fileContent.split('\n');

  for (let row = 0; row < lines.length; row++) {
    const line = lines[row];
    for (let col = 0; col < line.length; col++) {
      if (line[col] === '#') {
        const p: P = { x: row, y: col };
        map.set(key(p), true);
        elves.push(new Elf(p));
      }
    }
  }
}

parse();

for (let i = 0; ; i++) {
  if (!run()) {
    console.log(i + 1);
    break;
  }
}
