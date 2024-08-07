const fs = require('fs');
const readline = require('readline');

const SIDE = 5;
const SQUARE = SIDE * SIDE;

interface Space {
  [level: number]: boolean[];
}

function parse(): boolean[] {
  const input = fs.readFileSync('input.txt', 'utf8');
  const lines = input.split('\n');
  const res = new Array(SQUARE).fill(false);

  for (let row = 0; row < lines.length; row++) {
    const line = lines[row];
    for (let col = 0; col < SIDE; col++) {
      if (line[col] === '#') {
        res[row * SIDE + col] = true;
      }
    }
  }

  return res;
}

function next2(space: Space): Space {
  const newSpace: Space = {};

  let minLevel = Infinity;
  let maxLevel = -Infinity;
  for (const level in space) {
    if (parseInt(level) < minLevel) {
      minLevel = parseInt(level);
    }
    if (parseInt(level) > maxLevel) {
      maxLevel = parseInt(level);
    }
  }

  for (let level = minLevel - 1; level <= maxLevel + 1; level++) {
    newSpace[level] = new Array(SQUARE).fill(false);

    for (let cell = 0; cell < SQUARE; cell++) {
      if (cell === 12) {
        continue;
      }

      const row = Math.floor(cell / SIDE);
      const col = cell % SIDE;
      let neighbours = 0;

      if (row === 0) {
        if (infested(space, level - 1, 7)) {
          neighbours++;
        }
      }

      if (col === 0) {
        if (infested(space, level - 1, 11)) {
          neighbours++;
        }
      }

      if (col === 4) {
        if (infested(space, level - 1, 13)) {
          neighbours++;
        }
      }

      if (row === 4) {
        if (infested(space, level - 1, 17)) {
          neighbours++;
        }
      }

      if (cell === 7) {
        for (let i = 0; i < SIDE; i++) {
          if (infested(space, level + 1, i)) {
            neighbours++;
          }
        }
      }

      if (cell === 11) {
        for (let i = 0; i < SIDE; i++) {
          if (infested(space, level + 1, 5 * i)) {
            neighbours++;
          }
        }
      }

      if (cell === 13) {
        for (let i = 0; i < SIDE; i++) {
          if (infested(space, level + 1, 5 * i + SIDE - 1)) {
            neighbours++;
          }
        }
      }

      if (cell === 17) {
        for (let i = 0; i < SIDE; i++) {
          if (infested(space, level + 1, (SIDE - 1) * SIDE + i)) {
            neighbours++;
          }
        }
      }

      if (row > 0 && cell !== 17) {
        if (infested(space, level, cell - SIDE)) {
          neighbours++;
        }
      }

      if (col > 0 && cell !== 13) {
        if (infested(space, level, cell - 1)) {
          neighbours++;
        }
      }

      if (col < SIDE - 1 && cell !== 11) {
        if (infested(space, level, cell + 1)) {
          neighbours++;
        }
      }

      if (row < SIDE - 1 && cell !== 7) {
        if (infested(space, level, cell + SIDE)) {
          neighbours++;
        }
      }

      if (infested(space, level, cell) && neighbours !== 1) {
        newSpace[level][cell] = false;
        continue;
      }

      if (!infested(space, level, cell) && (neighbours === 1 || neighbours === 2)) {
        newSpace[level][cell] = true;
        continue;
      }

      newSpace[level][cell] = infested(space, level, cell);
    }
  }

  clean(newSpace);

  return newSpace;
}

function clean(space: Space) {
  let min = Infinity;
  let max = -Infinity;
  for (const level in space) {
    if (parseInt(level) < min) {
      min = parseInt(level);
    }
    if (parseInt(level) > max) {
      max = parseInt(level);
    }
  }

  let countMin = 0;
  let countMax = 0;
  for (let cell = 0; cell < SQUARE; cell++) {
    if (space[min][cell]) {
      countMin++;
    }
    if (space[max][cell]) {
      countMax++;
    }
  }

  if (countMin === 0) {
    delete space[min];
  }
  if (countMax === 0) {
    delete space[max];
  }
}

function infested(space: Space, level: number, cell: number): boolean {
  if (!space[level]) {
    return false;
  }
  return space[level][cell];
}

const input = parse();
let space: Space = { 0: input };

for (let i = 0; i < 200; i++) {
  space = next2(space);
}

let count = 0;
for (const level in space) {
  for (let cell = 0; cell < SQUARE; cell++) {
    if (space[level][cell]) {
      count++;
    }
  }
}

console.log(count);