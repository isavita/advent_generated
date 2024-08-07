import * as fs from 'fs';

type P = { X: number, Y: number };

class Elf {
  Pos: P;
  Moving: boolean;
  NextPos: P;

  constructor(pos: P) {
    this.Pos = pos;
    this.Moving = false;
    this.NextPos = pos;
  }

  AroundAllEmpty(): boolean {
    for (const d of Dirs) {
      const adj = { X: this.Pos.X + d.X, Y: this.Pos.Y + d.Y };
      if (Map[`${adj.X},${adj.Y}`]) return false;
    }
    return true;
  }

  ElfInDirection(wannaGo: number): boolean {
    for (let j = -1; j <= 1; j++) {
      const dxy = Dirs[(wannaGo + j + 8) % 8];
      const adj = { X: this.Pos.X + dxy.X, Y: this.Pos.Y + dxy.Y };
      if (Map[`${adj.X},${adj.Y}`]) return true;
    }
    return false;
  }
}

const N = 1, E = 3, S = 5, W = 7;

let Map: { [key: string]: boolean } = {};
let Elves: Elf[] = [];
let Order = [N, S, W, E];
let CurrDir = 0;
const Dirs = [
  { X: -1, Y: -1 }, { X: -1, Y: 0 }, { X: -1, Y: 1 },
  { X: 0, Y: 1 }, { X: 1, Y: 1 }, { X: 1, Y: 0 },
  { X: 1, Y: -1 }, { X: 0, Y: -1 }
];

function parse() {
  const input = fs.readFileSync('input.txt', 'utf8').split('\n');
  input.forEach((line, row) => {
    [...line].forEach((char, col) => {
      if (char === '#') {
        const p = { X: row, Y: col };
        Map[`${p.X},${p.Y}`] = true;
        Elves.push(new Elf(p));
      }
    });
  });
}

function run(): boolean {
  const proposes: { [key: string]: number } = {};

  Elves.forEach(e => {
    if (e.AroundAllEmpty()) return;

    for (let i = 0; i < 4; i++) {
      const dir = Order[(CurrDir + i) % 4];

      if (e.ElfInDirection(dir)) continue;

      const dxy = Dirs[dir];
      const dest = { X: e.Pos.X + dxy.X, Y: e.Pos.Y + dxy.Y };
      proposes[`${dest.X},${dest.Y}`] = (proposes[`${dest.X},${dest.Y}`] || 0) + 1;
      e.NextPos = dest;
      e.Moving = true;
      break;
    }
  });

  let someoneMoved = false;
  Elves.forEach(e => {
    if (!e.Moving) return;

    if (proposes[`${e.NextPos.X},${e.NextPos.Y}`] > 1) {
      e.Moving = false;
      return;
    }

    someoneMoved = true;
    delete Map[`${e.Pos.X},${e.Pos.Y}`];
    Map[`${e.NextPos.X},${e.NextPos.Y}`] = true;
    e.Pos = e.NextPos;
    e.Moving = false;
  });

  CurrDir = (CurrDir + 1) % 4;
  return someoneMoved;
}

function minMax(): [P, P] {
  let min = { X: 1 << 30, Y: 1 << 30 };
  let max = { X: -(1 << 30), Y: -(1 << 30) };

  for (const key in Map) {
    const [X, Y] = key.split(',').map(Number);
    if (X < min.X) min.X = X;
    if (Y < min.Y) min.Y = Y;
    if (X > max.X) max.X = X;
    if (Y > max.Y) max.Y = Y;
  }

  return [min, max];
}

parse();

for (let i = 0; i < 10; i++) {
  run();
}

const [min, max] = minMax();

let count = 0;
for (let x = min.X; x <= max.X; x++) {
  for (let y = min.Y; y <= max.Y; y++) {
    if (!Map[`${x},${y}`]) count++;
  }
}

console.log(count);